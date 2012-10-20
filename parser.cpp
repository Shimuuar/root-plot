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


// Accumulator for graphs
class AccumGraph : public LineAccum {
public:
    AccumGraph();
    virtual ~AccumGraph();
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);
protected:
    class Private;
    boost::scoped_ptr<Private> p;
};

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

PLineAccum makeAccumGraph() {
    return boost::shared_ptr<AccumGraph>( new AccumGraph() );
}

PLineAccum makeAccumPoly() {
    return boost::shared_ptr<AccumGraph>( new AccumPoly() );
}

PLineAccum makeAccumBarchart() {
    return boost::shared_ptr<AccumBarchart>( new AccumBarchart() );
}



// ================================================================
// ==== Line accumulator
// ================================================================

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


// ----------------------------------------------------------------

// Accumulator which does nothing
class NullAccum : public LineAccum {
public:
    virtual ~NullAccum() {}
    virtual bool flush(Plot*)                  { return true; }
    virtual bool feedLine(const std::string& ) { return true; }
};

// ----------------------------------------------------------------

struct AccumGraph::Private {
    Private() :
        mode( Unknown )
    {}
    enum Mode {
        Unknown,
        Col_1,              // Y   (X implicit [0,1...])
        Col_2,              // X,Y data
        Col_3,              // X,Y,ΔY
        Col_4               // X,Y,ΔX,ΔY
    };

    Mode mode;
    std::vector<double> xs, ys, dxs, dys;

    bool column_1(const std::string& str) {
        double y;
        bool   ok = 1 == sscanf(str.c_str(), "%lf", &y);
        if( ok ) {
            ys.push_back( y );
        }
        return ok;
    }
    bool column_2(const std::string& str ) {
        double x, y;
        bool   ok = 2 == sscanf(str.c_str(), "%lf %lf", &x, &y);
        if( ok ) {
            xs.push_back( x );
            ys.push_back( y );
        }
        return ok;
    }
    bool column_3(const std::string& str ) {
        double x, y, dy;
        bool   ok = 3 == sscanf(str.c_str(), "%lf %lf %lf", &x, &y, &dy);
        if( ok ) {
            xs.push_back(  x  );
            ys.push_back(  y  );
            dys.push_back( dy );
        }
        return ok;
    }
    bool column_4(const std::string& str ) {
        double x, y, dx,dy;
        bool   ok = 4 == sscanf(str.c_str(), "%lf %lf %lf %lf", &x, &y, &dx, &dy);
        if( ok ) {
            xs.push_back(  x  );
            ys.push_back(  y  );
            dxs.push_back( dx );
            dys.push_back( dy );
        }
        return ok;
    }
};

AccumGraph::AccumGraph() :
    p( new AccumGraph::Private )
{}

AccumGraph::~AccumGraph()
{}

bool AccumGraph::flush(Plot* plot) {
    size_t n;
    switch( p->mode ) {
    // 1 column data
    case Private::Col_1:
        n = p->ys.size();
        p->xs.resize( n );
        for( unsigned i = 0; i < n; i++ )
            p->xs[i] = i;
        // !! FALLTHROUGH !!
    // 2 column data
    case Private::Col_2:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraph( p->xs.size(), &(p->xs[0]), &(p->ys[0])) ) );
        return true;
    // 3 column data
    case Private::Col_3:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraphErrors( p->xs.size(), &(p->xs[0]), &(p->ys[0]), 0, &(p->dys[0])) ) );
        return true;
    // 4 column data
    case Private::Col_4:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraphErrors( p->xs.size(), &(p->xs[0]), &(p->ys[0]), &(p->dxs[0]), &(p->dys[0])) ) );
        return true;
    // Ooops
    case Private::Unknown: ;
    }
    return false;
}

bool AccumGraph::feedLine(const std::string& str) {
    switch( p->mode ) {
    // One column data
    case Private::Col_1:
        return p->column_1(str);
    // Two column data
    case Private::Col_2:
        return p->column_2(str);
    // Three column data
    case Private::Col_3:
        return p->column_3(str);
    // Four column data
    case Private::Col_4:
        return p->column_4(str);
    // Decide type of data
    case Private::Unknown:
             if ( p->column_4(str) ) { p->mode = Private::Col_4; }
        else if ( p->column_3(str) ) { p->mode = Private::Col_3; }
        else if ( p->column_2(str) ) { p->mode = Private::Col_2; }
        else if ( p->column_1(str) ) { p->mode = Private::Col_1; }
        return p->mode != Private::Unknown;
    };
    return false; // Unreachable
}



AccumPoly::AccumPoly()
{}

AccumPoly::~AccumPoly()
{}

bool AccumPoly::flush(Plot* plot) {
    switch( p->mode ) {
    case Private::Col_2:
        // Close outline manually
        if( p->ys.size() == 0 )
            return false;
        p->xs.push_back( p->xs[0] );
        p->ys.push_back( p->ys[0] );
        plot->pushObject(
            boost::make_shared<PlotPoly>(
                new TPolyLine( p->xs.size(), &(p->xs[0]), &(p->ys[0])) ) );
        return true;
    default:
        return false;
    }
}

AccumBarchart::AccumBarchart()
{}

AccumBarchart::~AccumBarchart()
{}

bool AccumBarchart::flush(Plot* plot) {
    switch( p->mode ) {
    case Private::Col_2:
        plot->pushObject(
            boost::make_shared<PlotBarChart>(
                new TGraph( p->xs.size(), &(p->xs[0]), &(p->ys[0])) ) );
        return true;
    default:
        return false;
    }
}



// ================================================================ //
PLineAccum makeNullAccum() {
    return boost::make_shared<NullAccum>();
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
