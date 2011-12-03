
%{

#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"

#include <boost/make_shared.hpp>


    
void yyerror(ParseParam, const char* err) {
    std::cerr << "rt-plot: " << err << std::endl;
}

static Plot::Color strToColor(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), toupper );
    Plot::Color c = Plot::BLACK;
    if( str == "WHITE" ) {
        c = Plot::WHITE;
    } else if( str == "BLACK"   ) {
        c = Plot::BLACK;
    } else if( str == "RED"     ) {
        c = Plot::RED;
    } else if( str == "GREEN"   ) {
        c = Plot::GREEN;
    } else if( str == "BLUE"    ) {
        c = Plot::BLUE;
    } else if( str == "YELLOW"  ) {
        c = Plot::YELLOW;
    } else if( str == "MAGENTA" ) {
        c = Plot::MAGENTA;
    } else if( str == "CYAN"    ) {
        c = Plot::CYAN;
    } else if( str == "FOREST"  ) {
        c = Plot::FOREST;
    } else if( str == "VIOLET"  ) {
        c = Plot::VIOLET;
    }
    return c;
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
%token TOK_DOUBLE
%token TOK_DASH

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
%token KW_VLINE
%token KW_HLINE

 // ================================================================
%%

input
  : TOK_WS line                 // May start from whitespace!
  | line

line // Top level statement
  : /* empty */
  | KW_CLEAR eol                { par.plot->clear(); }
  | KW_SET   TOK_WS set
  /* Plot commands */
  | KW_ADD   { par.clearPlot = false; } TOK_WS plot
  | KW_PLOT  { par.clearPlot = true;  } TOK_WS plot
  ;

plot // Plotting command
  : KW_GRAPH TOK_WS plot_graph
  | KW_HIST  TOK_WS plot_hist
  | KW_VLINE TOK_WS TOK_DOUBLE eol
    { par.plot->pushObject(
            boost::make_shared<PlotLine>( Plot::Vertical, boost::get<double>($3) ) );
    }
  | KW_HLINE TOK_WS TOK_DOUBLE eol
    { par.plot->pushObject(
            boost::make_shared<PlotLine>( Plot::Horizontal, boost::get<double>($3) ) );
    }
  
plot_graph // Plot graph
  : TOK_DASH eol
    { par.parser->accumulate<AccumGraph>(); }
  | TOK_STR eol
    { par.parser->readFromFile<AccumGraph>( boost::get<std::string>($1), par.plot ); }
plot_hist // Plot historam
  : TOK_DASH eol
    { std::cerr << "Histograms are not implemented\n"; }
  | TOK_STR eol
    { std::cerr << "Histograms are not implemented\n"; }


set
  : KW_LINE TOK_WS setLine

setLine
  : KW_WIDTH TOK_WS TOK_INT eol
    { par.plot->setLineWidth( boost::get<int>($3) ); std::cout << "ASDF\n"; }
  | KW_COLOR TOK_WS TOK_INT eol
    { par.plot->setLineColor( Plot::toColor( boost::get<int>($3)) ); }
  | KW_COLOR TOK_WS TOK_STR eol
    { par.plot->setLineColor( strToColor( boost::get<std::string>( $3 ) ) ); }


// End of line
eol
  : /* empty */
  | TOK_WS
  ;

%%
