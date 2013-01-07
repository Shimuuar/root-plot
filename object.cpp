
#include "object.hpp"

#include <cmath>
#include <algorithm>
#include <boost/make_shared.hpp>

#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TGraph2D.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TPolyLine.h>

#include "memory.hpp"



// ================================================================ //
// ==== Plot object
// ================================================================ //

RangeM PlotObject::xRange() const {
    return boost::optional<Range>();
}

RangeM PlotObject::yRange() const {
    return boost::optional<Range>();
}

RangeM PlotObject::zRange() const {
    return boost::optional<Range>();
}



// ================================================================ //
// ==== Histogram
// ================================================================ //

PlotHist::PlotHist(TH1* h) :
    hist(h),
    m_lineWidth( 1     ),
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
    if( m_lineWidth > 0 ) {
        hist->SetLineWidth( m_lineWidth );
    } else {
        hist->SetLineWidth( 1 );
        opt = "B " + opt;
    }
    hist->Draw( opt.c_str() );
}

void PlotHist::setLineWidth(int width) {
    m_lineWidth = width;
}

void PlotHist::setLineColor(int col) {
    hist->SetLineColor(col);
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
        yMax *= 1.05;
        if( yLLow < 1e308 )
            return Range( 0, yMax, yLLow / 3);
        else
            return Range( 0, yMax );
    }
}

TObject* PlotHist::getRootObject() {
    return hist.get();
}


// ================================================================ //
// ==== Graph

PlotGraph::PlotGraph(TGraph* g) :
    color ( Plot::BLACK      ),
    line  ( Plot::SolidLine  ),
    marker( Plot::NoMarker   ),
    errs  ( Plot::Crosshairs ),
    graph ( g )
{
    // Set sane default color
    graph->SetFillColor( 20 );
}

void PlotGraph::plotOn(Pad*) {
    std::string opts = " SAME";
    // Set line style
    switch( line ) {
    case Plot::SolidLine:
        opts = "L" + opts;
        break;
    case Plot::Splines:
        opts = "C" + opts;
        break;
    default: ;
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

void PlotGraph::setLineWidth(int width) {
    graph->SetLineWidth(width);
}

void PlotGraph::setLineStyle(Plot::LineStyle l) {
    line = l;
}

void PlotGraph::setMarkerStyle(Plot::MarkerStyle m) {
    marker = m;
}

void PlotGraph::setErrorStyle(Plot::ErrorsStyle e) {
    errs = e;
}

void PlotGraph::setLineColor(int col) {
    color = col;
    graph->SetLineColor(col);
}

void PlotGraph::setFillColor(int col) {
    color = col;
    graph->SetFillColor(col);
}

void PlotGraph::setFillStyle(int col) {
    graph->SetFillStyle(col);
}

static void range_with_errors(int n, double* xs, double* dx, double& lo, double& hi) {
    lo = xs[0] - dx[0];
    hi = xs[0] + dx[0];
    for(int i = 1; i < n; i++) {
        lo = std::min( lo, xs[i] - dx[i] );
        hi = std::max( hi, xs[i] + dx[i] );
    }
}

RangeM PlotGraph::xRange() const {
    double hi, lo;
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    TGraphErrors *graphE = dynamic_cast<TGraphErrors*>( &*graph );
    if( graphE ) {
        range_with_errors( n, graphE->GetX(), graphE->GetEX(), lo, hi);
    } else {
        double* xs = graph->GetX();
        hi         = *std::max_element(xs, xs+n);
        lo         = *std::min_element(xs, xs+n);
    }
    Range r( lo, hi );
    r.logLow = r.low;
    r.padRange( 0.03 );
    return boost::optional<Range>( r );
}

RangeM PlotGraph::yRange() const {
    double hi, lo;
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    TGraphErrors *graphE = dynamic_cast<TGraphErrors*>( &*graph );
    if( graphE ) {
        range_with_errors( n, graphE->GetY(), graphE->GetEY(), lo, hi);
    } else {
        double* ys = graph->GetY();
        hi         = *std::max_element(ys, ys+n);
        lo         = *std::min_element(ys, ys+n);
    }
    Range r( lo, hi );
    r.logLow = r.low;
    r.padRange( 0.03 );
    return boost::optional<Range>( r );
}

TObject* PlotGraph::getRootObject() {
    return graph.get();
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

// ================================================================ //
// ==== Poly

PlotPoly::PlotPoly(TPolyLine* g) :
    poly(g)
{
    setLineWidth( 0);
    setFillColor(20);
}

void PlotPoly::plotOn(Pad*) {
    // std::string opts = "SAME";
    poly->Draw( "F" );
    if( width > 0 ) {
        poly->SetLineWidth(width);
        poly->Draw(  );
    }
}

void PlotPoly::setLineWidth(int w) {
    width = w;
}

void PlotPoly::setLineColor(int col) {
    poly->SetLineColor(col);
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
    r.padRange( 0.03 );
    return boost::optional<Range>( r );
}

RangeM PlotPoly::yRange() const {
    int n = poly->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* ys   = poly->GetY();
    Range r( *std::min_element(ys, ys+n),
             *std::max_element(ys, ys+n) );
    r.padRange( 0.03 );
    return boost::optional<Range>( r );
}

TObject* PlotPoly::getRootObject() {
    return poly.get();
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
    RangeM rng = cxt->xRange();
    if( !rng )
        return;
    xs[0] = rng->low;
    xs[1] = rng->hi;
    ys[0] = k*xs[0] + b;
    ys[1] = k*xs[1] + b;
    // Draw
    graph = makeROOT<TGraph>(2, xs, ys);
    doDraw();
}

void PlotLine::plotVH(Pad* cxt) {
    // Fill arrays for graph
    double consts[2] = {x,x};
    double vars[2]   = {0,1};
    RangeM rng       =
        orientation == Plot::Vertical ? cxt->yRange() : cxt->xRange();
    if( rng ) {
        vars[0] = rng->low;
        vars[1] = rng->hi;
    }
    // Expand
    double delta = vars[1] - vars[0];
    vars[0] -= delta;
    vars[1] += delta;

    if( orientation == Plot::Vertical ) {
        graph = makeROOT<TGraph>(2, consts, vars);
    } else {
        graph = makeROOT<TGraph>(2, vars, consts);
    }
    doDraw();
}

void PlotLine::doDraw() {
    graph->SetLineWidth( width );
    graph->SetLineColor( color );
    graph->Draw("SAME L");
}

void PlotLine::setLineWidth(int w) {
    width = w;
}

void PlotLine::setLineColor(int col) {
    color = col;
}

TObject* PlotLine::getRootObject() {
    return graph.get();
}



// ================================================================ //
// ==== Band


void PlotBand::plotOn(Pad* cxt) {
    // Determine range
    double lo = 0;
    double hi = 1;
    RangeM rng       =
        orientation == Plot::Vertical ? cxt->yRange() : cxt->xRange();
    if( rng ) {
        lo = rng->low;
        hi = rng->hi;
    }

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
    poly->SetLineWidth( width     );
    poly->SetLineColor( color     );
    poly->Draw("F");
    if( width > 0 )
        poly->Draw("");
}

void PlotBand::setFillColor(int col) {
    fill = col;
}

void PlotBand::setFillStyle(int col) {
    fillStyle = col;
}

void PlotBand::setLineWidth(int w) {
    width = w;
}

void PlotBand::setLineColor(int col) {
    color = col;
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
