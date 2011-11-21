
#include "parser.hpp"
#include "parser.l.hpp"
int yyparse();

#include <ctype.h>
#include <boost/lexical_cast.hpp>

void parseLine(const std::string& str) {
    YY_BUFFER_STATE state;
    state = yy_scan_string( str.c_str() );

    if( yyparse( ) ) {
        std::cout << "BAD\n";
    } else {
        std::cout << "OK\n";
    }
    yy_delete_buffer( state );
}

int yyerror(char const* str) {
    return 0;
}

// ================================================================ //
Parser::Parser() :
    state(Command)
{
}


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
