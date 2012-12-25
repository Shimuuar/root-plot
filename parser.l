
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
doubleE  [-+]?[0-9]+[eE][-+]?[0-9]+
doublePE [-+]?[0-9]+\.[0-9]*[eE][-+]?[0-9]+
ws       [ \t]+
strlit   \"[^\"]*\"

%%%

 /* Keywords */
clear       { return KW_CLEAR;  }
on          { return KW_ON;     }
off         { return KW_OFF;    }
exit        { return KW_EXIT;   }
quit        { return KW_EXIT;   }
save        { return KW_SAVE;   }
object      { return KW_OBJECT; }
legend      { return KW_LEGEND; }

plot        { return KW_PLOT;     }
add         { return KW_ADD;      }
hist        { return KW_HIST;     }
graph       { return KW_GRAPH;    }
graph2D     { return KW_GRAPH2D;  }
barchart    { return KW_BARCHART; }
poly        { return KW_POLY;     }
vline       { return KW_VLINE;    }
hline       { return KW_HLINE;    }
vband       { return KW_VBAND;    }
hband       { return KW_HBAND;    }

set         { return KW_SET;    }
silent      { return KW_SILENT; }
title       { return KW_TITLE;  }
error       { return KW_ERROR;  }
cross       { return KW_CROSS;  }
band        { return KW_BAND;   }

xaxis       { return KW_XAXIS;  }
yaxis       { return KW_YAXIS;  }
zaxis       { return KW_ZAXIS;  }
log         { return KW_LOG;    }
label       { return KW_LABEL;  }
range       { return KW_RANGE;  }

line        { return KW_LINE;    }
width       { return KW_WIDTH;   }
style       { return KW_STYLE;   }
marker      { return KW_MARKER;  }
color       { return KW_COLOR;   }
fill        { return KW_FILL;    }
text        { return KW_TEXT;    }
box         { return KW_BOX;     }
contour     { return KW_CONTOUR; }
palette     { return KW_PALETTE; }
scatter     { return KW_SCATTER; }
grid        { return KW_GRID;    }
x           { return KW_X;       }
y           { return KW_Y;       }

 /* Literals */
{int}       { *yylval = boost::lexical_cast<int>   ( yytext ); return TOK_INT;    }
{doubleP}   { *yylval = boost::lexical_cast<double>( yytext ); return TOK_DOUBLE; }
{doubleE}   { *yylval = boost::lexical_cast<double>( yytext ); return TOK_DOUBLE; }
{doublePE}  { *yylval = boost::lexical_cast<double>( yytext ); return TOK_DOUBLE; }
{ws}        { return TOK_WS; }
{strlit}    { *yylval = std::string( yytext+1, strlen(yytext+1) - 1 ); return TOK_STR; }
-           { return TOK_DASH; }

 /* Lexing error */
.           { return TOK_BAD_LIT; }
<<EOF>>     {
    // Ensure that EOF matches only once
    static int once = 0;
    if( (once = !once) )
        return TOK_EOF;
    else
        return 0;
  }
%%%

