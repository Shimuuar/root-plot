
#include "parser.hpp"

#include <ctype.h>
#include <boost/lexical_cast.hpp>

Parser::Parser() :
    state(Command)
{
}


void Parser::feedLine(Plot* plot, const std::string& str) {
    switch( state ) {
    Command:
        procCommand(plot,str);
        break;
    Graph:
        procGraph(plot, str);
        break;
    }
}

void Parser::procCommand(Plot* plot, const std::string& str ) {
    LexedLine row;
    if( !lexLine(str, row) ) {
        std::cerr << "rt-plot: Bad line '" << str << "'\n";
        return;
    }
    // Line successfully lexed
    
}

void Parser::procGraph(Plot* plot, const std::string& str ) {
}
