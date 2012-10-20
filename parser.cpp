#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <ctype.h>
#include <sstream>
#include <fstream>
#include <boost/make_shared.hpp>

#include <TGraph.h>
#include <TGraphErrors.h>
#include <TPolyLine.h>


namespace {

// ================================================================
// Accumulator which does nothing
class NullAccum : public LineAccum {
public:
    virtual ~NullAccum() {}
    virtual bool flush(Plot*)                  { return true; }
    virtual bool feedLine(const std::string& ) { return true; }
};


// ================================================================
// Accumulator for graphs
class AccumGraph : public LineAccum {
public:
    virtual ~AccumGraph();
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);
protected:
    typedef std::vector< std::vector<double> > columns;

    columns cols;

    size_t  colSize()     { return cols[0].size();  }
    double* colPtr(int i) { return &( cols[i][0] ); }
};

AccumGraph::~AccumGraph()
{}

bool AccumGraph::flush(Plot* plot) {
    switch ( cols.size() ) {
    case 1: {
        size_t n = colSize();
        std::vector<double> xs;
        xs.resize( n );
        for( unsigned i = 0; i < n; i++ )
            xs[i] = i;
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraph( n, &(xs[0]), colPtr(0) ) ) );
        return true;
    }
    case 2:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraph( colSize(), colPtr(0), colPtr(1) ) ) );
        return true;
    case 3:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraphErrors( colSize(), colPtr(0), colPtr(1), 0, colPtr(2)) ) );
        return true;
    case 4:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraphErrors( colSize(), colPtr(0), colPtr(1), colPtr(2), colPtr(3)) ) );
        return true;
    default:
        return false;
    }
}

bool AccumGraph::feedLine(const std::string& str) {
    // Lexing buffer line
    std::vector<int> offs;
    std::string s = str;
    // Skip initial whitespaces;
    unsigned i = 0;
    while( i < s.length() && isspace(s[i]) )
        i++;
    // Start lexing
    bool wordEnded = true;
    for(; i < s.length(); i++) {
        if( isspace( s[i] ) ) {
            wordEnded = true;
            s[i] = '0';
        } else if (wordEnded) {
            offs.push_back( i );
        }
    }
    // Resize columns array if needed
    if( cols.size() == 0 ) {
        cols.resize( offs.size() );
    }
    if( cols.size() != offs.size() ) {
        return false;
    }
    // Read every number
    for( unsigned j = 0; j < offs.size(); j++) {
        double x;
        if( 1 != sscanf( s.c_str() + offs[j], "%lf", &x ) )
            return false;
        cols[j].push_back( x );
    }
    return true;
}


// Accumulator for barchart
class AccumBarchart : public AccumGraph {
public:
    AccumBarchart();
    virtual ~AccumBarchart();
    virtual bool flush(Plot*);
};


// Accumulator for polygons
class AccumPoly : public AccumGraph {
public:
    AccumPoly();
    virtual ~AccumPoly();
    virtual bool flush(Plot*);
};


AccumPoly::AccumPoly()
{}

AccumPoly::~AccumPoly()
{}

bool AccumPoly::flush(Plot* plot) {
    switch ( cols.size() ) {
    case 2: {
        if( colSize() == 0 )
            return false;
        std::vector<double>& xs = cols[0];
        std::vector<double>& ys = cols[1];
        xs.push_back( xs[0] );
        ys.push_back( ys[0] );
        plot->pushObject(
            boost::make_shared<PlotPoly>(
                new TPolyLine( xs.size(), &(xs[0]), &(ys[0])) ) );
        return true;
    }
    default:
        return false;
    }
}

AccumBarchart::AccumBarchart()
{}

AccumBarchart::~AccumBarchart()
{}

bool AccumBarchart::flush(Plot* plot) {
    switch( cols.size() ) {
    case 2:
        plot->pushObject(
            boost::make_shared<PlotBarChart>(
                new TGraph( colSize(), colPtr(0), colPtr(1) ) ) );
        return true;
    default:
        return false;
    }
}
}


// ================================================================ //
PLineAccum makeNullAccum() {
    return boost::make_shared<NullAccum>();
}

PLineAccum makeAccumGraph() {
    return boost::make_shared<AccumGraph>();
}

PLineAccum makeAccumPoly() {
    return boost::make_shared<AccumGraph>();
}

PLineAccum makeAccumBarchart() {
    return boost::make_shared<AccumBarchart>();
}

bool LineAccum::readFromFile(const std::string& fname, Plot* plot) {
    // Open file
    std::ifstream in( fname.c_str() );
    if( !in.good() )
        return false;
    // Read line-by line
    std::string str;
    while( !std::getline(in, str).eof() ) {
        if( ! feedLine( str ) )
            return false;
    }
    return flush( plot );
}

Parser::Parser()
{
}

void Parser::feedLine(Plot* plot, const std::string& str) {
    if( accum ) {
        // Did we hit end of data marker?
        bool endOfData = str.size() >= 3
                      && str[0] == '<' && str[1] == '<' && str[2] == '<';
        for(size_t i = 3; endOfData && i < str.size(); i++ )
            endOfData = endOfData && isspace( str[i] );

        if( endOfData ) {
            // Flush and delete accumulator
            if( ! accum->flush( plot ) ) {
                std::cerr << "rt-plot: cannot add inline data to the plot";
            }
            accum.reset();
        } else {
            // Push one line to accumulator
            if( !accum->feedLine(str) ) {
                std::cerr << "rt-plot: cannot parse inline data dropping to null parser\n";
                accum = makeNullAccum();
            }
        }
    } else {
        // We are parsing DSL
        YY_BUFFER_STATE state;
        state = yy_scan_string( str.c_str() );
        if( 0 != yyparse( ParseParam(this, plot) ) ) {
            std::cerr << "  in string: '" << str << "'\n";
        }
        // Parse completed
        yy_delete_buffer( state );
    }

    // Redraw everything
    if( !accum )
        plot->draw();
}
