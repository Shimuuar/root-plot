
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
#include <TROOT.h>
#include <TLegend.h>
#include <TPolyLine.h>
#include <TPaveText.h>



static boost::optional<double> joinLogLow( boost::optional<double> a, boost::optional<double> b) {
    if( ! a.is_initialized() )
        return b;
    if( ! b.is_initialized() )
        return a;
    return std::min( a.get(), b.get() );
}

static RangeM joinRange(const RangeM& r1, const RangeM& r2) {
    if( ! r1.is_initialized() )
        return r2;
    if( ! r2.is_initialized() )
        return r1;
    return Range( std::min(r1->low, r2->low) ,
                  std::max(r1->hi,  r2->hi ) ,
                  joinLogLow( r1->logLow, r2->logLow ) );
}

void Range::padRange(double eps) {
    double d = eps * (hi - low);
    low -= d;
    hi  += d;
}


// ================================================================ //
// ==== Pad

Pad::Pad(TPad* cnv) :
    m_canvas(cnv),
    m_errorList(),
    m_xLog( false ),
    m_yLog( false ),
    m_zLog( false )
{
    m_canvas->cd();
}

// Remove everything from canvas
void Pad::clearCanvas() {
    // Delete extra canvases. They could appear when one creates slice
    // TIter next( dynamic_cast<TList*>( gROOT->GetListOfCanvases() ) );
    // for(TCanvas *cnv; (cnv = dynamic_cast<TCanvas*>(next())); ) {
    //     if( cnv != m_canvas )
    //         delete cnv;
    // }
    m_canvas->Clear();
    m_canvas->cd();
    m_canvas->SetLogx( m_xLog );
    m_canvas->SetLogy( m_yLog );
    m_canvas->SetLogz( m_zLog );
}

void Pad::clear() {
    m_objStack.resize(0);
    m_errors.resize(0);
    m_gridX  = m_gridY = false;
    m_xLog   = m_yLog = m_zLog = false;
    m_title  = "";
    m_xLabel = boost::optional<std::string>();
    m_yLabel = boost::optional<std::string>();
    m_xLow   = boost::optional<double>();
    m_xHi    = boost::optional<double>();
    m_yLow   = boost::optional<double>();
    m_yHi    = boost::optional<double>();
    removeLegend();
    clearCanvas();
}

void Pad::draw() {
    // Remove everything from canvas
    clearCanvas();
    m_canvas->SetGrid( m_gridX, m_gridY );

    // Set ranges for graph
    double xs[2] = {0, 1};
    double ys[2] = {0, 1};

    RangeM rngX = xRange();
    if( rngX.is_initialized() ) {
        if( m_xLog && rngX->logLow.is_initialized() )
            xs[0] = rngX->logLow.get();
        else
            xs[0] = rngX->low;
        xs[1] = rngX->hi;
    }
    RangeM rngY = yRange();
    if( rngY.is_initialized() ) {
        if( m_yLog && rngY->logLow.is_initialized() )
            ys[0] = rngY->logLow.get();
        else
            ys[0] = rngY->low;
        ys[1] = rngY->hi;
    }

    // Setting batch mode removes flicker when title is not set
    // m_canvas->SetBatch( true );
    // Draw frame for the plots.
    TH1* hist = m_canvas->DrawFrame( xs[0], ys[0], xs[1], ys[1] );
    if( !!m_xLabel )
        hist->GetXaxis()->SetTitle( m_xLabel->c_str() );
    if( !!m_yLabel )
        hist->GetYaxis()->SetTitle( m_yLabel->c_str() );
    // Drawing title introduces flicker for some reason
    if( m_title.size() > 0 )
        hist->SetTitle( m_title.c_str() );

    // Draw all objects
    for( Stack::iterator o = m_objStack.begin(); o != m_objStack.end(); ++o) {
        (*o)->plotOn( this );
    }
    // Draw legend
    if( m_legend )
        m_legend->Draw();
    // Draw errors if there is any
    if( m_errors.size() != 0 ) {
        double yLo;
        if( m_yLog )
            yLo = sqrt( ys[0] * ys[1] );
        else
            yLo = 0.5 * (ys[1] + ys[0]);
        m_errorList = boost::make_shared<TPaveText>( xs[0], ys[1], xs[1], yLo );
        for( size_t i = 0; i < m_errors.size(); i++ ) {
            m_errorList->AddText( m_errors[i].c_str() );
        }
        m_errorList->Draw();
    }
}

void Pad::reportError(const std::string& str) {
    m_errors.push_back( str );
}

void Pad::save(const std::string& fname) {
    // FIXME: saving
    // draw(true);
    m_canvas->SaveAs(fname.c_str(), "Landscape");
}

void Pad::saveObj(const std::string& fname) {
    if( !m_objStack.empty() ) {
        TObject* o = m_objStack.back()->getRootObject();
        if( o )
            o->SaveAs(fname.c_str(), "Landscape");
    }
}

void Pad::pushObject(boost::shared_ptr<PlotObject> plot) {
    m_objStack.push_back( plot );
}

void Pad::setGrid(Plot::Axis axis, bool flag) {
    switch( axis ) {
    case Plot::X: m_gridX = flag; break;
    case Plot::Y: m_gridY = flag; break;
    default:;
    }
}

void Pad::setLabel(Plot::Axis axis, const std::string& label) {
    switch( axis ) {
    case Plot::X: m_xLabel = label; break;
    case Plot::Y: m_yLabel = label; break;
    case Plot::Z: break;
    }
}

void Pad::setLogScale(Plot::Axis axis, bool l) {
    switch( axis ) {
    case Plot::X: m_xLog = l; break;
    case Plot::Y: m_yLog = l; break;
    case Plot::Z: m_zLog = l; break;
    }
}

void Pad::setLineColor(int color) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineColor(color);
}

void Pad::setFillColor(int color) {
    if( !m_objStack.empty() )
        m_objStack.back()->setFillColor(color);
}

void Pad::setFillStyle(int s) {
    // Convert to ROOT encoding
    int style;
    if( s < 0 ) {
        style = 0;
    } else if( s == 0 ) {
        style = 1001;
    } else {
        style = 3000 + (s % 1000);
    }
    if( !m_objStack.empty() )
        m_objStack.back()->setFillStyle(style);
}

void Pad::setLineWidth(int width) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineWidth(width);
}

void Pad::setLineStyle(Plot::LineStyle l) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineStyle( l );
}

void Pad::setMarkerStyle(Plot::MarkerStyle m) {
    if( !m_objStack.empty() )
        m_objStack.back()->setMarkerStyle( m );
}

void Pad::setErrorStyle(Plot::ErrorsStyle m) {
    if( !m_objStack.empty() )
        m_objStack.back()->setErrorStyle( m );
}

void Pad::setHistText( bool txt ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistText( txt );
}

void Pad::setHistScatter( bool scat ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistScatter( scat );
}

void Pad::setHistContour( int n ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistContour( n );
}

void Pad::setHistColor( bool c ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistColor( c );
}

void Pad::setHistBox( bool c ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistBox( c );
}

void Pad::setHistPalette( bool p ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistPalette( p );
}

static RangeM axisRange(boost::optional<double> low,
                        boost::optional<double> hi,
                        const Pad::Stack objs,
                        Plot::Axis axis
    )
{
    // We have full range. It's fixed and need not any tweaking
    if( low.is_initialized() && hi.is_initialized() )
        return boost::optional<Range>( Range( low.get(), hi.get() ) );
    // Estimate range
    RangeM rng;
    for(Pad::Stack::const_iterator i = objs.begin(); i != objs.end(); ++i ) {
        switch( axis ) {
        case Plot::X: rng = joinRange(rng, (*i)->xRange()); break;
        case Plot::Y: rng = joinRange(rng, (*i)->yRange()); break;
        case Plot::Z: rng = joinRange(rng, (*i)->zRange()); break;
        }
    }
    // Tweak range if needed
    if( rng.is_initialized() ) {
        if( low.is_initialized() )
            rng->low = low.get();
        if( hi.is_initialized() )
            rng->hi  = hi.get();
    }
    return rng;
}

RangeM Pad::xRange() const {
    return axisRange( m_xLow, m_xHi, m_objStack, Plot::X );
}

RangeM Pad::yRange() const {
    return axisRange( m_yLow, m_yHi, m_objStack, Plot::Y );
}

void Pad::setRange(Plot::Axis axis, boost::optional<double> a, boost::optional<double> b) {
    switch( axis ) {
    case Plot::X:
        m_xLow = a;
        m_xHi  = b;
        return;
    case Plot::Y:
        m_yLow = a;
        m_yHi  = b;
        return;
    case Plot::Z:
        // FIXME: treat Z!
        return;
    }
}

void Pad::setRange(Plot::Axis axis) {
    switch( axis ) {
    case Plot::X:
        m_xLow.reset();
        m_xHi. reset();
        return;
    case Plot::Y:
        m_yLow.reset();
        m_yHi. reset();
        return;
    case Plot::Z:
        // FIXME: treat Z!
        return;
    }
}

void Pad::removeLegend() {
    m_legend.reset();
}

void Pad::addLegend(double x1, double y1, double x2, double y2) {
    m_legend = boost::make_shared<TLegend>(x1, y1, x2, y2);
}

void Pad::addLegendString(const std::string& str) {
    if( m_legend )
        m_legend->AddEntry(new TObject(), str.c_str(), "");
}

void Pad::addPlotToLegend(const std::string& str) {
    if( m_legend && !m_objStack.empty() ) {
        TObject* obj = m_objStack.back()->getRootObject();
        if( obj )
            m_legend->AddEntry(obj, str.c_str() );
    }
}



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
    TH2* h2d = dynamic_cast<TH2*>( &(*hist) );
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
    return &( *hist );
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
    r.padRange( 0.03 );
    return boost::optional<Range>( r );
}

TObject* PlotGraph::getRootObject() {
    return &( *graph );
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
    return &( *graph );
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
    return &( *poly );
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
    graph = boost::make_shared<TGraph>(2, xs, ys);
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
        graph = boost::make_shared<TGraph>(2, consts, vars);
    } else {
        graph = boost::make_shared<TGraph>(2, vars, consts);
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
        poly = boost::shared_ptr<TPolyLine>( new TPolyLine(4, xs, ys, "") );
    } else {
        double ys[4] = {x1,x1,x2,x2};
        double xs[4] = {lo,hi,hi,lo};
        poly = boost::shared_ptr<TPolyLine>( new TPolyLine(4, xs, ys, "") );
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
