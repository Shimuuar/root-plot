
%{

#include "parser.hpp"
#include "parser.l.hpp"

#include "object.hpp"
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
using namespace boost::lambda;

void yyerror(Closure&, const char*);

%}

%define api.pure
%parse-param {Closure& clos}

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

 // ================================================================
%%

input
  : TOK_WS line
  | line

line
  : /* empty */
  | KW_CLEAR eol           { clos = bind(&Plot::clear, _1 ); }
  | KW_SET   TOK_WS set
  /* Plot commands */
  | KW_ADD   TOK_WS plot
  | KW_PLOT  TOK_WS plot
  ;

plot : eol
set  : KW_LINE TOK_WS setLine

setLine
  : KW_WIDTH TOK_WS TOK_INT eol { clos = bind( &Plot::setLineWidth, _1, boost::get<int>($3) ); }
  | KW_COLOR TOK_WS TOK_INT eol { clos = bind( &Plot::setLineColor, _1, static_cast<Plot::Color>( boost::get<int>($3)) ); }

// End of line
eol
  : /* empty */
  | TOK_WS
  ;

%%

void yyerror(Closure&, const char*) {
    return ;
}
