
%{

//    typedef void* yyscan_t;
#include "parser.hpp"
#include "parser.l.hpp"

// #define YY_DECL int lexLineWorker(LexedLine& res)
static int yyerror(const char* str);

%}

%define api.pure

%start line

 // Literals
%token TOK_WS
%token TOK_STR
%token TOK_INT

 // Keywords
%token KW_CLEAR
%token KW_SET

%token KW_ADD
%token KW_PLOT

%%

line : ;
     | KW_CLEAR eol          { std::cout << "CLEAR!\n"; }
     | KW_ADD  TOK_WS plot
     | KW_PLOT TOK_WS plot
     ;

plot : eol

// End of line
eol  : /* empty */
     | TOK_WS
     ;

%%

       // int yyparse(void *param);
void parseLine(const std::string& str) {
    YY_BUFFER_STATE state;
    state = yy_scan_string( str.c_str() );

    // if ( yylex_init( &p ) ) {   // couldn't initialize
    //     return;
    // }

    // state = yy_scan_string(str->c_str(), p);

    if( yyparse( ) ) {
        std::cout << "BAD\n";
    } else {
        std::cout << "OK\n";
    }
    yy_delete_buffer( state );
    // if ( yyparse(&p) ) {    // error parsing
    //     return NULL;
    // }

    // yy_delete_buffer(state, p.scanner);
    // yylex_destroy( p );

    return;
    
}

static int yyerror(const char* str) {
    return 0;
}
