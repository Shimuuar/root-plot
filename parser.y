
%{

#include "parser.hpp"
#include "parser.l.hpp"

#include "object.hpp"

void yyerror(ParseParam, const char* err) {
    std::cerr << "rt-plot: " << err << std::endl;
}

%}

%define api.pure
%parse-param {ParseParam par}

 // Root rule
%start input

 // Literals
%token TOK_WS
%token TOK_STR
%token TOK_INT

 // Keywords
%token KW_CLEAR
 // SET
%token KW_SET
%token KW_LINE
%token KW_COLOR
%token KW_WIDTH

 // PLOT
%token KW_ADD
%token KW_PLOT
%token KW_HIST
%token KW_GRAPH


 // ================================================================
%%

input
  : TOK_WS line
  | line

line
  : /* empty */
| KW_CLEAR eol           { }//clos = bind(&Plot::clear, _1 ); }
  | KW_SET   TOK_WS set
  /* Plot commands */
  | KW_ADD   { par.clearPlot = false; } TOK_WS plot
  | KW_PLOT  { par.clearPlot = true;  } TOK_WS plot
  ;

plot
  : KW_GRAPH eol
  | KW_HIST  eol

set  : KW_LINE TOK_WS setLine

setLine
  : KW_WIDTH TOK_WS TOK_INT eol
  { par.plot->setLineWidth( boost::get<int>($3) ); std::cout << "ASDF\n"; }
  | KW_COLOR TOK_WS TOK_INT eol
  { par.plot->setLineColor( static_cast<Plot::Color>( boost::get<int>($3)) ); }

// End of line
eol
  : /* empty */
  | TOK_WS
  ;

%%
