
#include "parser.hpp"
#include "parser.l.hpp"
// int yyparse();

#include <ctype.h>
#include <boost/noncopyable.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>

// #include "closure.hpp"


// Accumulator for lines
class LineAccum {
public:
    virtual ~LineAccum() {}

    // Return true if accumulator is ready and performed action
    virtual bool flush(Plot* plot) = 0;
    // Feed line to accumulator
    virtual void feedLine(const std::string& str) = 0;
};

class AccGraph : public LineAccum {
public:
    virtual ~AccGraph() {}

    virtual bool flush(Plot*);
    virtual void feedLine(const std::string& str);
private:
    std::vector<double> xs, ys;
};

bool AccGraph::flush(Plot* plot) {
    return true;
}

void AccGraph::feedLine(const std::string& str) {
    
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
        yy_delete_buffer( state );        
    }
}

// ================================================================ //
// Language #define

void Parser::procCommand(Plot* plot, const std::string& str ) {
    // LexedLine row;
    // if( !lexLine(str, row) ) {
    //     std::cerr << "rt-plot: Bad line '" << str << "'\n";
    //     return;
    // }
    // Line successfully lexed
    
}

void Parser::procGraph(Plot* plot, const std::string& str ) {
}
