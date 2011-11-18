
#include "parser.hpp"

#include <ctype.h>
#include <boost/lexical_cast.hpp>

Parser::Parser() :
    state(Command)
{
}


void parseLine(const std::string& str);
void Parser::feedLine(Plot* plot, const std::string& str) {
    parseLine(str);
    return ;
    
    switch( state ) {
    case Command:
        procCommand(plot,str);
        break;
    case Graph:
        procGraph(plot, str);
        break;
    }
}

// ================================================================ //
// Language #define

bool keyword(const std::string& kwd, LexedLine& row) {
    if( row.size() == 0 )
        return false;
    Keyword* str = boost::get<Keyword>( &row.front() );
    return  str != 0
        && *str == kwd
        && (row.size() == 1 || boost::get<WhiteSpace>( &row[1] ) != 0 );
};

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
