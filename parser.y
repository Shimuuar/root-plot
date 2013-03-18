
%{

#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <TApplication.h>
#include <boost/make_shared.hpp>


using boost::get;
    
void yyerror(ParseParam, const char* err) {
    std::cerr << "rt-plot: " << err << std::endl;
}

static Plot::Color strToColor( std::string str) {
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

static Plot::LineStyle strToLine( std::string str ) {
    std::transform(str.begin(), str.end(), str.begin(), toupper );
    Plot::LineStyle l = Plot::SolidLine;
    if( str == "NO" ) {
        l = Plot::NoLine;
    } else if( str == "LINE" ) {
        l = Plot::SolidLine;
    } else if( str == "SPLINES" ) {
        l = Plot::Splines;
    }
    return l;
}

static Plot::MarkerStyle strToMarker( std::string str ) {
    std::transform(str.begin(), str.end(), str.begin(), toupper );
    Plot::MarkerStyle m = Plot::NoMarker;
    if( str == "NO" ) {
        m = Plot::NoMarker;
    } else if( str == "DOT"  || str == "." ) {
        m = Plot::MarkerDot;
    } else if( str == "PLUS" || str == "+" ) {
        m = Plot::MarkerPlus;
    } else if( str == "STAR" || str == "*" ) {
        m = Plot::MarkerStar;
    } else if( str == "O" ) {
        m = Plot::MarkerO;
    } else if( str == "X" ) {
        m = Plot::MarkerX;
    }
    return m;
}

static void setPalette( Plot* p, std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), toupper );
    if( str == "DEEP SEA" ) {
        p->setPalette( Plot::DeepSea );
    } else if( str == "GREY SCALE" || str == "GRAY SCALE" ) {
        p->setPalette( Plot::GreyScale );
    } else if( str == "DARK BODY" || str == "BLACK BODY" ) {
        p->setPalette( Plot::BlackBody );
    } else if( str == "BLUE YELLOW" ) {
        p->setPalette( Plot::BlueYellow );
    } else {
        p->setPalette( Plot::DeepSea );
    }
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

// Set parser for inline data
static void setParserStdin(PLineAccum acc, ParseParam& par) {
    if( par.clearPlot )
        par.plot->clear();
    par.parser->accumulate( acc );
}

// Set parser for data
static void setParserFile(PLineAccum acc, ParseParam& par, const Token& tok) {
    if( par.clearPlot )
        par.plot->clear();   
    acc->readFromFile(get<std::string>(tok), par.plot);
}

// Set grid for both axes
static void setGrid(ParseParam& par, bool x, bool y) {
    if( par() ) {
        par.pad->setGrid(Plot::X, x);
        par.pad->setGrid(Plot::Y, y);
    }
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
 // Special token end of input
%token TOK_EOF
 // Bad literal. It's not present in any rules so it's automatically parse error.
%token TOK_BAD_LIT

 // Keywords
%token KW_CLEAR
%token KW_ON
%token KW_OFF
%token KW_EXIT
%token KW_SAVE
%token KW_OBJECT
%token KW_LEGEND
%token KW_PAD
%token KW_ROW
%token KW_COLUMN
%token KW_CANVAS
%token KW_SIZE

 // SET
%token KW_SET
%token KW_SILENT
%token KW_TITLE
%token KW_ERROR
%token KW_CROSS
%token KW_BAND

%token KW_LINE
%token KW_COLOR
%token KW_WIDTH
%token KW_STYLE
%token KW_MARKER
%token KW_FILL
%token KW_TEXT
%token KW_FORMAT
%token KW_BOX
%token KW_SCATTER
%token KW_CONTOUR
%token KW_PALETTE

%token KW_XAXIS
%token KW_YAXIS
%token KW_ZAXIS
%token KW_LOG
%token KW_LABEL
%token KW_RANGE

%token KW_GRID
%token KW_X
%token KW_Y

 // PLOT
%token KW_ADD
%token KW_PLOT
%token KW_END
%token KW_HIST
%token KW_GRAPH
%token KW_GRAPH2D
%token KW_BARCHART
%token KW_POLY
%token KW_VLINE
%token KW_HLINE
%token KW_ABLINE
%token KW_VBAND
%token KW_HBAND

 // ================================================================
%%

input
  : TOK_WS line                 // May start from whitespace!
  | line

line // Top level statement
  : /* empty */
  | KW_CLEAR eol                { par.plot->clear(); }
  | KW_EXIT  eol                { gApplication->Terminate(); }
  | KW_SAVE  TOK_WS TOK_STR eol
      { par.plot->save( get<std::string>($3) ); }
  | KW_SET   TOK_WS set
  /* Plot commands. Add is used to add rows/columns/pads as well */
  | KW_ADD   { par.clearPlot = false; } TOK_WS addPlot
  | KW_PLOT  { par.clearPlot = true;  } TOK_WS plot
  /* End row/column/pad commands  */
  | KW_END TOK_WS KW_ROW    eol { par.plot->completeRow(); }
  | KW_END TOK_WS KW_COLUMN eol { par.plot->completeRow(); }
  | KW_END TOK_WS KW_PAD    eol { par.plot->completePad(); }
  /* Legend */
  | KW_LEGEND TOK_WS legend
  ;

addPlot
  : KW_ROW    eol               { par.plot->addRow( Plot::Horizontal                ); }
  | KW_ROW    TOK_WS double eol { par.plot->addRow( Plot::Horizontal, getDouble($3) ); }
  | KW_COLUMN eol               { par.plot->addRow( Plot::Vertical                  ); }
  | KW_COLUMN TOK_WS double eol { par.plot->addRow( Plot::Vertical,   getDouble($3) ); }
  | KW_PAD               eol { par.plot->addPad( 1 );                }
  | KW_PAD TOK_WS double eol { par.plot->addPad( getDouble( $3 ) );  }
  | plot

plot // Plotting command
  : KW_GRAPH    TOK_WS plot_graph
  | KW_GRAPH2D  TOK_WS plot_graph2D
  | KW_BARCHART TOK_WS plot_barchart
  | KW_POLY     TOK_WS plot_poly
  | KW_HIST     TOK_WS plot_hist
    // Horizonal/vertical lines
  | KW_VLINE TOK_WS double eol
    { par.plot->pushObject( boost::make_shared<PlotLine>( Plot::Vertical,   getDouble($3) ) ); }
  | KW_HLINE TOK_WS double eol
    { par.plot->pushObject( boost::make_shared<PlotLine>( Plot::Horizontal, getDouble($3) ) ); }
  | KW_ABLINE TOK_WS double TOK_WS double eol
    { par.plot->pushObject( boost::make_shared<PlotLine>( getDouble($3), getDouble($5) ) ); }
    // Horizontal/vertical bands
  | KW_VBAND TOK_WS double TOK_WS double eol
    { par.plot->pushObject(
          boost::make_shared<PlotBand>(
              Plot::Vertical, getDouble($3), getDouble( $5 ) ) ); }
  | KW_HBAND TOK_WS double TOK_WS double eol
    { par.plot->pushObject(
          boost::make_shared<PlotBand>(
              Plot::Horizontal, getDouble($3), getDouble( $5 ) ) ); }

// Plot graph
plot_graph
  : TOK_DASH eol { setParserStdin( makeAccumGraph(), par     ); }
  | TOK_STR  eol { setParserFile ( makeAccumGraph(), par, $1 ); }
// Plot 2D graph
plot_graph2D
  : TOK_DASH eol { setParserStdin( makeAccumGraph2D(), par     ); }
  | TOK_STR  eol { setParserFile ( makeAccumGraph2D(), par, $1 ); }
// Barchart
plot_barchart
  : TOK_DASH eol { setParserStdin( makeAccumBarchart(), par     ); }
  | TOK_STR  eol { setParserFile ( makeAccumBarchart(), par, $1 ); }
// Plot polygon
plot_poly
  : TOK_DASH eol { setParserStdin( makeAccumPoly(), par     ); }
  | TOK_STR  eol { setParserFile ( makeAccumPoly(), par, $1 ); }
// Plot histogram
plot_hist
  : TOK_DASH eol { setParserStdin( makeAccumHist(), par     ); }
  | TOK_STR  eol { setParserFile ( makeAccumHist(), par, $1 ); }

// Legend
legend
  : TOK_DASH eol
    { if( par() ) par.pad->removeLegend(); }
  | KW_ADD TOK_WS double TOK_WS double TOK_WS double TOK_WS double eol
    {
        if( par() )
            par.pad->addLegend(
                getDouble( $3 ),
                getDouble( $5 ),
                getDouble( $7 ),
                getDouble( $9 ) );
    }
  | KW_ADD TOK_WS TOK_STR eol
    { if( par() ) par.pad->addPlotToLegend( get<std::string>( $3 ) ); }
  | KW_ADD TOK_WS KW_LABEL TOK_WS TOK_STR eol
    { if( par() ) par.pad->addLegendString( get<std::string>( $5 ) ); }
  | KW_ADD TOK_WS KW_LABEL TOK_WS TOK_STR TOK_WS TOK_STR eol
    { if( par() ) par.pad->addLegendString( get<std::string>( $5 ), get<std::string>( $7 ) ); }


set
  : KW_LINE    TOK_WS setLine
  | KW_FILL    TOK_WS setFill
  | KW_HIST    TOK_WS setHist
  | KW_SILENT  TOK_WS KW_ON    eol { par.plot->setSilent( true  ); }
  | KW_SILENT  TOK_WS KW_OFF   eol { par.plot->setSilent( false ); }
  | KW_TITLE   TOK_WS TOK_STR  eol { if( par() ) par.pad->setTitle( get<std::string>( $3 ) ); }
  | KW_ERROR   TOK_WS TOK_DASH eol { if( par() ) par.pad->setErrorStyle( Plot::NoErrors   ); }
  | KW_ERROR   TOK_WS KW_CROSS eol { if( par() ) par.pad->setErrorStyle( Plot::Crosshairs ); }
  | KW_ERROR   TOK_WS KW_BAND  eol { if( par() ) par.pad->setErrorStyle( Plot::ErrorBand  ); }
  | KW_PALETTE TOK_WS TOK_STR  eol { setPalette( par.plot, get<std::string>( $3 ) ); }
  // Grid
  | KW_GRID                 eol { setGrid(par, true,  true ); }
  | KW_GRID   TOK_WS KW_ON  eol { setGrid(par, true,  true ); }
  | KW_GRID   TOK_WS KW_OFF eol { setGrid(par, false, false); }
  // X
  | KW_GRID TOK_WS KW_X               eol { if( par() ) par.pad->setGrid(Plot::X, true);  }
  | KW_GRID TOK_WS KW_X TOK_WS KW_ON  eol { if( par() ) par.pad->setGrid(Plot::X, true);  }
  | KW_GRID TOK_WS KW_X TOK_WS KW_OFF eol { if( par() ) par.pad->setGrid(Plot::X, false); }
  // Y
  | KW_GRID TOK_WS KW_Y               eol { if( par() ) par.pad->setGrid(Plot::Y, true);  }
  | KW_GRID TOK_WS KW_Y TOK_WS KW_ON  eol { if( par() ) par.pad->setGrid(Plot::Y, true);  }
  | KW_GRID TOK_WS KW_Y TOK_WS KW_OFF eol { if( par() ) par.pad->setGrid(Plot::Y, false); }
  // Axes
  | KW_XAXIS  { par.axis = Plot::X; } TOK_WS setAxis
  | KW_YAXIS  { par.axis = Plot::Y; } TOK_WS setAxis
  | KW_ZAXIS  { par.axis = Plot::Z; } TOK_WS setAxis
  // Canvas
  | KW_CANVAS TOK_WS KW_SIZE TOK_WS TOK_INT TOK_WS TOK_INT eol
    { par.plot->setCanvasSize( get<int>($5), get<int>( $7) ); }

setAxis
  : KW_LABEL TOK_WS TOK_DASH eol
    { if( par() ) par.pad->setLabel(Plot::Axis(par.axis), ""); }
  | KW_LABEL TOK_WS TOK_STR  eol
    { if( par() ) par.pad->setLabel(Plot::Axis(par.axis), get<std::string>( $3 )); }
  | KW_LOG   onOff
    { if( par() ) par.pad->setLogScale(Plot::Axis(par.axis), par.onOff ); }
  | KW_RANGE TOK_WS TOK_DASH eol
    { if( par() ) par.pad->setRange(Plot::Axis(par.axis)); }
  | KW_RANGE TOK_WS double TOK_WS double eol
    {
        if( par() )
            par.pad->setRange(
                Plot::Axis(par.axis),
                boost::optional<double>( getDouble( $3 )),
                boost::optional<double>( getDouble( $5 ))
                );
    }
  | KW_RANGE TOK_WS TOK_DASH TOK_WS double eol
    {
        if( par() )
            par.pad->setRange(
                Plot::Axis(par.axis),
                boost::optional<double>(),
                boost::optional<double>( getDouble( $5 ))
                );
    }
  | KW_RANGE TOK_WS double TOK_WS TOK_DASH eol
    {
        if( par() )
            par.pad->setRange(
                Plot::Axis(par.axis),
                boost::optional<double>( getDouble( $3 )),
                boost::optional<double>()
                );
    }
  | KW_RANGE TOK_WS TOK_DASH TOK_WS TOK_DASH eol
    {
        if( par() )
            par.pad->setRange(
                Plot::Axis(par.axis),
                boost::optional<double>(),
                boost::optional<double>()
                );
    }


// Line options
setLine
  : KW_WIDTH  TOK_WS TOK_INT eol
    { if( par() ) par.pad->setLineWidth( get<int>($3) ); }
  | KW_COLOR  TOK_WS TOK_INT eol
    { if( par() ) par.pad->setLineColor( get<int>($3) ); }
  | KW_COLOR  TOK_WS TOK_STR eol
    { if( par() ) par.pad->setLineColor( strToColor( get<std::string>( $3 ) ) ); }
  | KW_STYLE  TOK_WS TOK_STR eol
    { if( par() ) par.pad->setLineStyle( strToLine( get<std::string>( $3 ) ) ); }
  | KW_MARKER TOK_WS TOK_STR eol
    { if( par() ) par.pad->setMarkerStyle( strToMarker( get<std::string>( $3 ) ) ); }

// Fill options
setFill
  : KW_COLOR TOK_WS TOK_INT eol
    { if( par() ) par.pad->setFillColor( get<int>($3) ); }
  | KW_COLOR TOK_WS TOK_STR eol
    { if( par() ) par.pad->setFillColor( strToColor( get<std::string>( $3 ) ) ); }
  | KW_STYLE TOK_WS TOK_INT eol
    { if( par() ) par.pad->setFillStyle( get<int>( $3 ) ); }

// Histogram options
setHist
  : KW_TEXT    onOff
    { if( par() ) par.pad->setHistText( par.onOff ); }
  | KW_TEXT TOK_WS KW_FORMAT TOK_WS TOK_DASH eol
    { if( par() ) par.pad->setHistTextFmt( -1 ); }
  | KW_TEXT TOK_WS KW_FORMAT TOK_WS TOK_INT eol
    { if( par() ) par.pad->setHistTextFmt( get<int>( $5 ) ); }
  | KW_COLOR   onOff
    { if( par() ) par.pad->setHistColor( par.onOff ); }
  | KW_BOX     onOff
    { if( par() ) par.pad->setHistBox( par.onOff ); }
  | KW_SCATTER onOff
    { if( par() ) par.pad->setHistScatter( par.onOff ); }
  | KW_CONTOUR onOff
    { if( par() ) par.pad->setHistContour( par.onOff ? 10 : -1 ); } // 10 is default number of contours
  | KW_CONTOUR TOK_WS TOK_INT eol
    { if( par() ) par.pad->setHistContour( get<int>( $3 ) ); }
  | KW_PALETTE onOff
    { if( par() ) par.pad->setHistPalette( par.onOff ); }

// End of line
eol
  : TOK_EOF
  | TOK_WS  TOK_EOF
  ;
// Double valued token. Value sould be retrieved using getDouble
double
  : TOK_INT
  | TOK_DOUBLE
// ON/OFF end of line switch
onOff
  : eol               { par.onOff = true;  }
  | TOK_WS KW_ON  eol { par.onOff = true;  }
  | TOK_WS KW_OFF eol { par.onOff = false; }
%%
