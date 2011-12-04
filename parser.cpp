#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <ctype.h>
#include <sstream>
#include <fstream>
#include <boost/make_shared.hpp>

#include <TGraph.h>


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

bool AccumGraph::flush(Plot* plot) {
    plot->pushObject(
        boost::make_shared<PlotGraph>(
            new TGraph(xs.size(), &xs[0], &ys[0]) ) );
    return true;
}

bool AccumGraph::feedLine(const std::string& str) {
    std::istringstream is( str );
    double x, y;
    is >> x >> y;
    if( is.fail() ) {
        return false;
    } else {
        xs.push_back(x);
        ys.push_back(y);
        return true;
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
        int st = yyparse( ParseParam(this, plot) );
        std::cout << "Status = " << st << std::endl;
        // Parse completed
        yy_delete_buffer( state );
    }

    // Redraw everything
    if( !accum )
        plot->draw();
}
