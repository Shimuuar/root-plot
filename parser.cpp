#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"

#include <ctype.h>
#include <vector>
#include <boost/noncopyable.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>

#include <TGraph.h>



// Incremental parser for embedded data. E.g. graphs, histograms etc.
class LineAccum {
public:
    virtual ~LineAccum() {}

    // If accumulator is ready it adds its contents to the plot and
    // returns true. Otherwise just returns false.
    virtual bool flush(Plot* plot) = 0;
    // Feed line to accumulator. If line is parsed sucesfully returns
    // true
    virtual bool feedLine(const std::string& str) = 0;
};

// Accumulator for graphs
class AccumGraph : public LineAccum {
public:
    virtual ~AccumGraph() {}
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);
private:
    std::vector<double> xs, ys;
};

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
 

// ================================================================ //
Parser::Parser() 
{
}


void Parser::feedLine(Plot* plot, const std::string& str) {
    if( accum ) {
        
    } else {
        YY_BUFFER_STATE state;
        state = yy_scan_string( str.c_str() );
        plot->setLineWidth(2);
        int st = yyparse( ParseParam(this, plot) );
        std::cout << "Status = " << st << std::endl;
        // Parse completed
        yy_delete_buffer( state );
    }
}
