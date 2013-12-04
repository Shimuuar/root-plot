
#include "object.hpp"

#include <cmath>
#include <algorithm>
#include <limits>
#include <boost/make_shared.hpp>
#include <boost/format.hpp>

#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TGraph2D.h>
#include <TGraphErrors.h>
#include <TGraphAsymmErrors.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TPolyLine.h>
#include <TExec.h>

#include "memory.hpp"



// ================================================================ //
// ==== Plot object
// ================================================================ //

PlotObject::PlotObject() :
    isAutorange(true),
    // Default values
    m_lineColor( Plot::BLACK     ),
    m_lineWidth( 1               ),
    m_lineStyle( Plot::Solid     ),
    m_lineType ( Plot::SolidLine )
{}

RangeM PlotObject::xRange() const {
    return boost::optional<Range>();
}

RangeM PlotObject::yRange() const {
    return boost::optional<Range>();
}

RangeM PlotObject::zRange() const {
    return boost::optional<Range>();
}

bool PlotObject::haveFill() const {
    return false;
}


// ================================================================ //
// ==== Histogram
// ================================================================ //

PlotHist::PlotHist(TH1* h) :
    hist(h),
    m_cmd( makeROOT<TExec>("CMD", "gStyle->SetPaintTextFormat(\"g\");") ),
    m_text     ( false ),
    m_scatter  ( false ),
    m_box      ( false ),
    m_nCont    ( -1    ),
    m_color    ( false ),
    m_palette  ( false )
{
    h->SetStats( 0 );
}

void PlotHist::plotOn(Pad*) {
    std::string opt = "SAME";
    if( m_text )
        opt += " TEXT";
    if( m_scatter )
        opt += " SCAT";
    if( m_box )
        opt += " BOX";
    if( m_color ) {
        opt += " COL";
        if( m_palette ) {
            opt += " Z";
        }
    }
    // If line width is set to zero draw histogram as bar chart
    hist->SetLineColor( m_lineColor );
    if( m_lineWidth > 0 ) {
        hist->SetLineWidth( m_lineWidth );
    } else {
        hist->SetLineWidth( 1 );
        opt = "B " + opt;
    }
    // Draw histogram
    m_cmd->Draw();
    hist->Draw( opt.c_str() );
}

void PlotHist::setHistTextFmt(int n) {
    if( n < 0 )
        m_cmd = makeROOT<TExec>("CMD", "gStyle->SetPaintTextFormat(\"g\");");
    else {
        std::string cmd = (boost::format("gStyle->SetPaintTextFormat(\".%df\");") % n).str();
        m_cmd = makeROOT<TExec>("CMD", cmd.c_str() );
    }
}

void PlotHist::setFillColor(int col) {
    hist->SetFillColor(col);
}

void PlotHist::setFillStyle(int col) {
    hist->SetFillStyle(col);
}

RangeM PlotHist::xRange() const {
    return Range( hist->GetXaxis()->GetXmin(),
                  hist->GetXaxis()->GetXmax() );
}

RangeM PlotHist::yRange() const {
    TH2* h2d = dynamic_cast<TH2*>( hist.get() );
    if( h2d ) {
        // 2D histogram
        return Range( hist->GetYaxis()->GetXmin(),
                      hist->GetYaxis()->GetXmax() );
    } else {
        // 1D histogram
        int    n     = hist->GetNbinsX();
        double yMax  = 0;
        double yLLow = 1e308;
        for( int i = 1; i <= n; i++ ) {
            double bin = hist->GetBinContent(i);
            yMax = std::max( yMax, bin );
            if( bin > 0 )
                yLLow = std::min( yLLow, bin );
        }
        Range rng(0, yMax);
        if( yLLow < 1e308 )
            rng.logLow = yLLow / 2;
        rng.wantPadHi = true;
        return rng;
    }
}

TObject* PlotHist::getRootObject() {
    return hist.get();
}

bool PlotHist::haveFill() const {
    return true;
}


// ================================================================ //
// ==== Graph

PlotGraph::PlotGraph(TGraph* g) :
    color    ( Plot::BLACK      ),
    marker   ( Plot::NoMarker   ),
    errs     ( Plot::Crosshairs ),
    graph    ( g )
{
    // Set sane default color
    graph->SetFillColor( 20 );
}

void PlotGraph::plotOn(Pad*) {
    std::string opts = " SAME";
    // Set line style
    switch( m_lineType ) {
    case Plot::SolidLine:
        opts = "L" + opts;
        break;
    case Plot::Splines:
        opts = "C" + opts;
        break;
    default: ;
    }
    // FIXME: set line type
    switch( m_lineStyle ) {
    case Plot::Solid:   graph->SetLineStyle( 1 ); break;
    case Plot::Dashed:  graph->SetLineStyle( 2 ); break;
    case Plot::Dotted:  graph->SetLineStyle( 3 ); break;
    case Plot::Dashdot: graph->SetLineStyle( 4 ); break;
    }
    // Set marker style
    if( marker != Plot::NoMarker ) {
        opts = "P" + opts;
        graph->SetMarkerStyle( marker );
        graph->SetMarkerColor( color  );
    }
    // Draw errors differently
    switch( errs ) {
    case Plot::NoErrors:
        // If we don't want to draw any errors
        graph->Draw( ("X"+opts).c_str() );
        break;
    case Plot::Crosshairs:
        // No special treatment needed
        graph->Draw( opts.c_str() );
        break;
    case Plot::ErrorBand:
        // This case is truly special. ROOT doesn't draw central line
        // so we need to create a separate plot for that
        graph->Draw( "3 SAME" );
        if( graph->GetLineWidth() > 0 )
            graph->Draw( ("X"+opts).c_str() );
        break;
    }
}

void PlotGraph::setMarkerStyle(Plot::MarkerStyle m) {
    marker = m;
}

void PlotGraph::setErrorStyle(Plot::ErrorsStyle e) {
    errs = e;
}

void PlotGraph::setFillColor(int col) {
    color = col;
    graph->SetFillColor(col);
}

void PlotGraph::setFillStyle(int col) {
    graph->SetFillStyle(col);
}

// Helpers for calculations of automatic ranges
static double err(double* xs, int n) {
    return xs ? xs[n] : 0;
}
static boost::optional<Range>
range_with_errors(int n, double* xs, double* ldx, double* udx)
{
    assert(n > 0 && "Expecting positive number");
    double lo    =  std::numeric_limits<double>::infinity();
    double hi    = -std::numeric_limits<double>::infinity();
    double logLo = -1;

    for(int i = 0; i < n; i++) {
        double loX = xs[i] - err(ldx,i);
        double hiX = xs[i] + err(udx,i);
        
        lo    = std::min( lo, loX );
        hi    = std::max( hi, hiX );
        // For minimum at log scale we first try value with error and
        // then value without error.
        if( loX > 0 && loX < logLo ) {
            logLo = loX;
        } else if( xs[i] > 0 && xs[i] < logLo ) {
            logLo = xs[i];
        }
    }
    Range r( lo, hi );
    // Minimum point for log scale is set as minimum coordinate or if
    // it's negative as half if minimal positive coordinate
    if( logLo > 0 && lo <= 0 )
        r.logLow = logLo / 2;
    else
        r.logLow = r.low;
    r.wantPadLow = true;
    r.wantPadHi  = true;
    return boost::optional<Range>( r );
}

RangeM PlotGraph::xRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    if( TGraphErrors *graphE = dynamic_cast<TGraphErrors*>( &*graph ) ) {
        return range_with_errors(
            n, graphE->GetX(), graphE->GetEX(), graphE->GetEX());
    }
    if( TGraphAsymmErrors *graphE = dynamic_cast<TGraphAsymmErrors*>( &*graph ) ) {
        return range_with_errors(
            n, graphE->GetX(), graphE->GetEXlow(), graphE->GetEXhigh());
    }
    return range_with_errors(n, graph->GetX(), 0, 0);
}

RangeM PlotGraph::yRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    if( TGraphErrors *graphE = dynamic_cast<TGraphErrors*>( &*graph ) ) {
        return range_with_errors(
            n, graphE->GetY(), graphE->GetEY(), graphE->GetEY());
    }
    if( TGraphAsymmErrors *graphE = dynamic_cast<TGraphAsymmErrors*>( &*graph ) ) {
        return range_with_errors(
            n, graphE->GetY(), graphE->GetEYlow(), graphE->GetEYhigh());
    }
    return range_with_errors(n, graph->GetY(), 0, 0);
}

TObject* PlotGraph::getRootObject() {
    return graph.get();
}

bool PlotGraph::haveFill() const {
    return false;
}


// ================================================================ //
// ==== 2D graph

PlotGraph2D::PlotGraph2D(TGraph2D* g) :
    graph ( g )
{}

void PlotGraph2D::plotOn(Pad*) {
    std::string opts = "SURF1";
    graph->Draw( opts.c_str() );
}

RangeM PlotGraph2D::xRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    double* xs    = graph->GetX();
    double  hi    = *std::max_element(xs, xs+n);
    double  lo    = *std::min_element(xs, xs+n);
    return boost::optional<Range>( Range(lo, hi) );
}

RangeM PlotGraph2D::yRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    double* ys    = graph->GetY();
    double  hi    = *std::max_element(ys, ys+n);
    double  lo    = *std::min_element(ys, ys+n);
    return boost::optional<Range>( Range(lo, hi) );
}

TObject* PlotGraph2D::getRootObject() {
    return graph.get();
}

bool PlotGraph2D::haveFill() const {
    return false;
}



// ================================================================ //
// ==== BarChart

PlotBarChart::PlotBarChart( TGraph* g ) :
    PlotGraph( g )
{
    graph->SetFillColor( 38 );
}

void PlotBarChart::plotOn(Pad*) {
    std::string opts = "B SAME";
    graph->Draw( opts.c_str() );
}

RangeM PlotBarChart::xRange() const {
    RangeM m = PlotGraph::xRange();
    int    n = graph->GetN();
    if( m.is_initialized() && n > 1 ) {
        double* xs = graph->GetX();
        m->low -= 0.5 * abs( xs[1]   - xs[0]   );
        m->hi  += 0.5 * abs( xs[n-1] - xs[n-2] );
    }
    return m;
}

RangeM PlotBarChart::yRange() const {
    RangeM m = PlotGraph::yRange();
    if( m.is_initialized() ) {
        m->low = std::min( 0.0, m->low );
    }
    return m;
}

void PlotBarChart::setFillColor(int c) {
    graph->SetFillColor( c );
}

void PlotBarChart::setFillStyle(int c) {
    graph->SetFillStyle( c );
}

bool PlotBarChart::haveFill() const {
    return true;
}


// ================================================================ //
// ==== Poly

PlotPoly::PlotPoly(TPolyLine* g) :
    poly(g)
{
    m_lineWidth  = 0;
    setFillColor(20);
}

void PlotPoly::plotOn(Pad*) {
    // Draw filled polygon
    poly->Draw( "F" );
    // If we want to draw border we need to draw border separately.
    if( m_lineWidth > 0 ) {
        poly->SetLineWidth( m_lineWidth );
        poly->Draw(  );
    }
}

void PlotPoly::setFillColor(int col) {
    poly->SetFillColor(col);
}

void PlotPoly::setFillStyle(int col) {
    poly->SetFillStyle(col);
}

RangeM PlotPoly::xRange() const {
    int n = poly->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* xs    = poly->GetX();
    Range r( *std::min_element(xs, xs+n),
             *std::max_element(xs, xs+n) );
    r.wantPadLow = true;
    r.wantPadHi  = true;
    return boost::optional<Range>( r );
}

RangeM PlotPoly::yRange() const {
    int n = poly->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* ys   = poly->GetY();
    Range r( *std::min_element(ys, ys+n),
             *std::max_element(ys, ys+n) );
    r.wantPadLow = true;
    r.wantPadHi  = true;
    return boost::optional<Range>( r );
}

TObject* PlotPoly::getRootObject() {
    return poly.get();
}

bool PlotPoly::haveFill() const {
    return true;
}



// ================================================================ //
// ==== Line

void PlotLine::plotOn(Pad* cxt) {
    if( abline )
        plotAB( cxt );
    else
        plotVH( cxt );
}

void PlotLine::plotAB(Pad* cxt) {
    double xs[2];
    double ys[2];
    // Set coordinates
    std::pair<double,double> rng = cxt->xRange();
    xs[0] = rng.first;
    xs[1] = rng.second;
    ys[0] = k*xs[0] + b;
    ys[1] = k*xs[1] + b;
    // Draw
    graph = makeROOT<TGraph>(2, xs, ys);
    doDraw();
}

void PlotLine::plotVH(Pad* cxt) {
    // Fill arrays for graph
    std::pair<double,double> rng =
        orientation == Plot::Vertical ? cxt->yRange() : cxt->xRange();
    double consts[2] = {x,x};
    double vars[2]   = {rng.first, rng.second};


    if( orientation == Plot::Vertical ) {
        graph = makeROOT<TGraph>(2, consts, vars);
    } else {
        graph = makeROOT<TGraph>(2, vars, consts);
    }
    doDraw();
}

void PlotLine::doDraw() {
    graph->SetLineWidth( m_lineWidth );
    graph->SetLineColor( m_lineColor );
    graph->SetFillColor( m_fill      );
    graph->SetFillStyle( m_fillSt    );
    graph->Draw("SAME L");
}

void PlotLine::setFillColor(int col) {
    m_fill = col;
}

void PlotLine::setFillStyle(int col) {
    m_fillSt = col;
}

TObject* PlotLine::getRootObject() {
    return graph.get();
}

bool PlotLine::haveFill() const {
    return false;
}


// ================================================================ //
// ==== Band


void PlotBand::plotOn(Pad* cxt) {
    // Determine range
    std::pair<double,double> rng =
        orientation == Plot::Vertical ? cxt->yRange() : cxt->xRange();
    double lo = rng.first;
    double hi = rng.second;

    if( orientation == Plot::Vertical ) {
        double xs[4] = {x1,x1,x2,x2};
        double ys[4] = {lo,hi,hi,lo};
        poly = makeROOT<TPolyLine>(4, xs, ys, "");
    } else {
        double ys[4] = {x1,x1,x2,x2};
        double xs[4] = {lo,hi,hi,lo};
        poly = makeROOT<TPolyLine>(4, xs, ys, "");
    }
    poly->SetFillColor( fill      );
    poly->SetFillStyle( fillStyle );
    poly->SetLineWidth( m_lineWidth );
    poly->SetLineColor( m_lineColor );
    // If we want to draw polygon with border we need to draw border
    // separately
    poly->Draw("F");
    if( m_lineWidth > 0 )
        poly->Draw("");
}

void PlotBand::setFillColor(int col) {
    fill = col;
}

void PlotBand::setFillStyle(int col) {
    fillStyle = col;
}

RangeM PlotBand::xRange() const {
    switch( orientation ) {
    case Plot::Vertical:
        return Range(x1,x2);
    default:
        return boost::optional<Range>();
    }
}

RangeM PlotBand::yRange() const {
    switch( orientation ) {
    case Plot::Vertical:
        return boost::optional<Range>();
    default:
        return Range(x1,x2);
    }
}

bool PlotBand::haveFill() const {
    return true;
}
