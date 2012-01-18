
%{

#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <TApplication.h>
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

// Pattern match over Token to retrieve double value
struct GetDoubleVisitor : public boost::static_visitor<double> {
    double operator()(int i)              const { return i; }
    double operator()(double x)           const { return x; }
    double operator()(const std::string&) const { throw std::logic_error("Impossible happened"); }
};
static double getDouble(const Token& tok) {
    return boost::apply_visitor(GetDoubleVisitor(), tok);
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
%token KW_ON
%token KW_OFF
%token KW_EXIT
%token KW_SAVE
%token KW_LEGEND

 // SET
%token KW_SET
%token KW_SILENT
%token KW_TITLE

%token KW_LINE
%token KW_COLOR
%token KW_WIDTH
%token KW_FILL
%token KW_TEXT
%token KW_BOX
%token KW_SCATTER
%token KW_CONTOUR

%token KW_XAXIS
%token KW_YAXIS
%token KW_ZAXIS
%token KW_LOG
%token KW_LABEL
%token KW_RANGE

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
  | KW_EXIT  eol                { gApplication->Terminate(); }
  | KW_SAVE  TOK_WS TOK_STR eol { par.plot->save( boost::get<std::string>($3) ); }
  | KW_SET   TOK_WS set
  /* Plot commands */
  | KW_ADD   { par.clearPlot = false; } TOK_WS plot
  | KW_PLOT  { par.clearPlot = true;  } TOK_WS plot
  /* Legend */
  | KW_LEGEND TOK_WS legend
  ;

plot // Plotting command
  : KW_GRAPH TOK_WS plot_graph
  | KW_HIST  TOK_WS plot_hist
    // Horizonal/vertical lines
  | KW_VLINE TOK_WS double eol
    { par.plot->pushObject( boost::make_shared<PlotLine>( Plot::Vertical,   getDouble($3) ) ); }
  | KW_HLINE TOK_WS double eol
    { par.plot->pushObject( boost::make_shared<PlotLine>( Plot::Horizontal, getDouble($3) ) ); }
  
// Plot graph
plot_graph
  : TOK_DASH eol
    {
        if( par.clearPlot )
            par.plot->clear();
        par.parser->accumulate<AccumGraph>();
    }
  | TOK_STR eol
    {
        if( par.clearPlot )
            par.plot->clear();
        par.parser->readFromFile<AccumGraph>( boost::get<std::string>($1), par.plot );
    }
// Plot historam
plot_hist
  : TOK_DASH eol
    {
        if( par.clearPlot )
            par.plot->clear();
        par.parser->accumulate<AccumHist>();
    }
  | TOK_STR eol
    {
        if( par.clearPlot )
            par.plot->clear();
        par.parser->readFromFile<AccumHist>( boost::get<std::string>($1), par.plot );
    }

// Legend
legend
  : TOK_DASH eol
    { par.plot->removeLegend(); }
  | KW_ADD TOK_WS double TOK_WS double TOK_WS double TOK_WS double eol
    {
        par.plot->addLegend(
            getDouble( $3 ),
            getDouble( $5 ),
            getDouble( $7 ),
            getDouble( $9 ) );
    }
  | KW_ADD TOK_WS TOK_STR eol
    { par.plot->addPlotToLegend( boost::get<std::string>( $3 ) ); }
  | KW_ADD TOK_WS KW_LABEL TOK_WS TOK_STR eol
    { par.plot->addLegendString( boost::get<std::string>( $5 ) ); }


set
  : KW_LINE   TOK_WS setLine
  | KW_FILL   TOK_WS setFill
  | KW_HIST   TOK_WS setHist
  | KW_SILENT TOK_WS KW_ON   eol   { par.plot->setSilent( true  ); }
  | KW_SILENT TOK_WS KW_OFF  eol   { par.plot->setSilent( false ); }
  | KW_TITLE  TOK_WS TOK_STR eol   { par.plot->setTitle( boost::get<std::string>( $3 ) ); }
  // Axes
  | KW_XAXIS  { par.axis = Plot::X; } TOK_WS setAxis
  | KW_YAXIS  { par.axis = Plot::Y; } TOK_WS setAxis
  | KW_ZAXIS  { par.axis = Plot::Z; } TOK_WS setAxis

setAxis
  : KW_LABEL TOK_WS TOK_DASH eol
    { par.plot->setLabel(Plot::Axis(par.axis), ""); }
  | KW_LABEL TOK_WS TOK_STR  eol
    { par.plot->setLabel(Plot::Axis(par.axis), boost::get<std::string>( $3 )); }
  | KW_LOG   onOff
    { par.plot->setLogScale(Plot::Axis(par.axis), par.onOff ); }
  | KW_RANGE TOK_WS TOK_DASH eol
    { par.plot->setRange(Plot::Axis(par.axis)); }
  | KW_RANGE TOK_WS double TOK_WS double eol
    {
        par.plot->setRange(
            Plot::Axis(par.axis),
            getDouble( $3 ),
            getDouble( $5 )
            );
    }

// Line options
setLine
  : KW_WIDTH TOK_WS TOK_INT eol
    { par.plot->setLineWidth( boost::get<int>($3) ); }
  | KW_COLOR TOK_WS TOK_INT eol
    { par.plot->setLineColor( Plot::toColor( boost::get<int>($3)) ); }
  | KW_COLOR TOK_WS TOK_STR eol
    { par.plot->setLineColor( strToColor( boost::get<std::string>( $3 ) ) ); }

// Fill options
setFill
  : KW_COLOR TOK_WS TOK_INT eol
    { par.plot->setFillColor( Plot::toColor( boost::get<int>($3)) ); }
  | KW_COLOR TOK_WS TOK_STR eol
    { par.plot->setFillColor( strToColor( boost::get<std::string>( $3 ) ) ); }

// Histogram options
setHist
  : KW_TEXT    onOff
    { par.plot->setHistText( par.onOff ); }
  | KW_COLOR   onOff
    { par.plot->setHistColor( par.onOff ); }
  | KW_BOX     onOff
    { par.plot->setHistColor( par.onOff ); }
  | KW_SCATTER onOff
    { par.plot->setHistScatter( par.onOff ); }
  | KW_CONTOUR onOff
    { par.plot->setHistContour( par.onOff ? 10 : -1 ); }
  | KW_CONTOUR TOK_WS TOK_INT eol
    { par.plot->setHistContour( boost::get<int>( $3 ) ); }

// End of line
eol
  : /* empty */
  | TOK_WS
  ;
// Double valued token. Value sould be retrieved using getDouble
double
  : TOK_INT
  | TOK_DOUBLE
// ON/OFF end of line switch
onOff
  : eol               { par.onOff = true;  }
  | TOK_WS KW_ON eol  { par.onOff = true;  }
  | TOK_WS KW_OFF eol { par.onOff = false; }
%%
