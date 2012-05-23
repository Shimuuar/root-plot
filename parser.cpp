#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <ctype.h>
#include <sstream>
#include <fstream>
#include <boost/make_shared.hpp>

#include <TGraph.h>
#include <TPolyLine.h>



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
        OneColumn,
        TwoColumn
    };

    Mode mode;
    std::vector<double> xs, ys;

    bool oneColumn(const std::string& str) {
        double y;
        bool   ok = 1 == sscanf(str.c_str(), "%lf", &y);
        if( ok ) {
            ys.push_back( y );
        }
        return ok;
    }
    bool twoColumn(const std::string& str ) {
        double x, y;
        bool   ok = 2 == sscanf(str.c_str(), "%lf %lf", &x, &y);
        if( ok ) {
            xs.push_back( x );
            ys.push_back( y );
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
    // One column data
    case Private::OneColumn:
        n = p->ys.size();
        p->xs.resize( n );
        for( unsigned i = 0; i < n; i++ )
            p->xs[i] = i;
        // !! FALLTHROUGH !!
    // Two column data
    case Private::TwoColumn:
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraph( p->xs.size(), &(p->xs[0]), &(p->ys[0])) ) );
        return true;
    // Ooops
    case Private::Unknown: ;
    }
    return false;
}

bool AccumGraph::feedLine(const std::string& str) {
    switch( p->mode ) {
    // One column data
    case Private::OneColumn:
        return p->oneColumn(str);
    // Two column data
    case Private::TwoColumn:
        return p->twoColumn(str);
    // Decide type of data
    case Private::Unknown:
        if( p->twoColumn(str) ) {
            p->mode = Private::TwoColumn;
        } else if( p->oneColumn(str) ) {
            p->mode = Private::OneColumn;
        }
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
    case Private::TwoColumn:
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



// ================================================================ //
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
                accum = boost::make_shared<NullAccum>();
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
