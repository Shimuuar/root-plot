
%{
#include "parser.hpp"
#include "parser.tab.h"

#include <boost/lexical_cast.hpp>
%}

%option nounput noyywrap never-interactive
%option bison-bridge

ident    [a-zA-Z][a-zA-Z0-9]*
int      [-+]?[0-9]+
ws       [ \t]+
strlit   \"[^\"]*\"

%%%

 /* Keywords */
clear       { return KW_CLEAR; }
plot        { return KW_PLOT;  }
add         { return KW_ADD;   }
set         { return KW_SET;   }
line        { return KW_LINE;  }
width       { return KW_WIDTH; }
color       { return KW_COLOR; }

 /* Literals */
{int}       { *yylval = boost::lexical_cast<int>( yytext ); return TOK_INT; }
{ws}        { return TOK_WS; }
{strlit}    { *yylval = std::string( yytext+1, strlen(yytext+1) - 1 ); return TOK_STR; }

%%%

/* bool lexLine(const std::string& str, LexedLine& res) { */
/*     try { */
/*         res.clear(); */
/*         // Set up flex */
/*         yy_scan_string( str.c_str() ); */
/*         lexLineWorker( res ); */
/*         yy_delete_buffer( YY_CURRENT_BUFFER ); */
/*     } catch( ... ) { */
/*         res.clear(); */
/*         return false; */
/*     } */
/*     return true; */
/* } */
