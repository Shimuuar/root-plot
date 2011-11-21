
%{

//    typedef void* yyscan_t;
#include "parser.hpp"
#include "parser.l.hpp"

// #define YY_DECL int lexLineWorker(LexedLine& res)
int yyerror(const char* str);

%}

%define api.pure

 // Root rule
%start input

 // Literals
%token TOK_WS
%token TOK_STR
%token TOK_INT

 // Keywords
%token KW_CLEAR
%token KW_SET

%token KW_ADD
%token KW_PLOT

 // ================================================================
%%

input : TOK_WS line
      | line

line : ;
     | KW_CLEAR eol          { std::cout << "CLEAR!\n"; }
     | KW_SET  TOK_WS set
     /* Plot commands */
     | KW_ADD  TOK_WS plot
     | KW_PLOT TOK_WS plot
     ;

plot : eol
set  : eol

// End of line
eol  : /* empty */
     | TOK_WS
     ;

%%
