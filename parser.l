
%{
#include "parser.hpp"
#include "parser.tab.h"

#include <boost/lexical_cast.hpp>
%}

%option nounput noyywrap never-interactive
%option bison-bridge

ident    [a-zA-Z][a-zA-Z0-9]*
int      [-+]?[0-9]+
doubleP  [-+]?[0-9]+(\.[0-9]*)?
doubleE  [-+]?[0-9]+[eE][-+]+[0-9]+
doublePE [-+]?[0-9]+\.[0-9]*[eE][-+]+[0-9]+
ws       [ \t]+
strlit   \"[^\"]*\"

%%%

 /* Keywords */
clear       { return KW_CLEAR;  }
on          { return KW_ON;     }
off         { return KW_OFF;    }
exit        { return KW_EXIT;   }

plot        { return KW_PLOT;   }
add         { return KW_ADD;    }
hist        { return KW_HIST;   }
graph       { return KW_GRAPH;  }
vline       { return KW_VLINE;  }
hline       { return KW_HLINE;  }

set         { return KW_SET;    }
silent      { return KW_SILENT; }
title       { return KW_TITLE;  }
line        { return KW_LINE;   }
width       { return KW_WIDTH;  }
color       { return KW_COLOR;  }

 /* Literals */
{int}       { *yylval = boost::lexical_cast<int>   ( yytext ); return TOK_INT;    }
{doubleP}   { *yylval = boost::lexical_cast<double>( yytext ); return TOK_DOUBLE; }
{doubleE}   { *yylval = boost::lexical_cast<double>( yytext ); return TOK_DOUBLE; }
{doublePE}  { *yylval = boost::lexical_cast<double>( yytext ); return TOK_DOUBLE; }
{ws}        { return TOK_WS; }
{strlit}    { *yylval = std::string( yytext+1, strlen(yytext+1) - 1 ); return TOK_STR; }
-           { return TOK_DASH; }

%%%
