
#include "object.hpp"

#include <algorithm>
#include <boost/make_shared.hpp>

#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TCanvas.h>
#include <TROOT.h>
#include <TLegend.h>
#include <TPolyLine.h>


static RangeM joinRange(const RangeM& r1, const RangeM& r2) {
    if( ! r1.is_initialized() )
        return r2;
    if( ! r2.is_initialized() )
        return r1;
    return Range( std::min(r1->low, r2->low) ,
                  std::max(r1->hi,  r2->hi ) );
}

// ================================================================ //
// ==== Plot

Plot::Plot(TCanvas* cnv) :
    m_canvas(cnv),
    m_xLog( false ),
    m_yLog( false ),
    m_zLog( false ),
    m_isSilent(false)
{
    m_canvas->cd();
}

// Remove everything from canvas
void Plot::clearCanvas() {
    // Delete extra canvases. They could appear when one creates slice
    TIter next( dynamic_cast<TList*>( gROOT->GetListOfCanvases() ) );
    for(TCanvas *cnv; (cnv = dynamic_cast<TCanvas*>(next())); ) {
        if( cnv != m_canvas )
            delete cnv;
    }
    m_canvas->Clear();
    m_canvas->cd();
    m_canvas->SetLogx( m_xLog );
    m_canvas->SetLogy( m_yLog );
    m_canvas->SetLogz( m_zLog );
}

void Plot::clear() {
    m_objStack.resize(0);
    m_xLog   = m_yLog = m_zLog = false;
    m_title  = "";
    m_xLabel = boost::optional<std::string>();
    m_yLabel = boost::optional<std::string>();
    m_xRange = boost::optional<Range>();
    m_yRange = boost::optional<Range>();
    removeLegend();
    clearCanvas();
}

void Plot::draw(bool force) {
    if( !force && m_isSilent )
        return;
    // Remove everything from canvas
    clearCanvas();

    // Set ranges for graph
    double xs[2] = {0, 1};
    double ys[2] = {0, 1};
    
    RangeM rngX = xRange();
    if( rngX.is_initialized() ) {
        xs[0] = rngX->low;
        xs[1] = rngX->hi;
    }
    RangeM rngY = yRange();
    if( rngY.is_initialized() ) {
        ys[0] = rngY->low;
        ys[1] = rngY->hi;
    }

    // Create invisible graph which holds axis for the plot. It's
    // required because ROOT do not allow to resize axes arbitralily
    // and one have to set them correctly upfront.
    //
    // On plus side axes are stored locally.    
    m_axisGraph = boost::make_shared<TGraph>(2,xs,ys);
    m_axisGraph->SetLineColor( Plot::WHITE );
    m_axisGraph->GetXaxis()->SetRangeUser( xs[0], xs[1] );
    m_axisGraph->GetYaxis()->SetRangeUser( ys[0], ys[1] );
    if( !!m_xLabel ) 
        m_axisGraph->GetXaxis()->SetTitle( m_xLabel->c_str() );
    if( !!m_yLabel )
        m_axisGraph->GetYaxis()->SetTitle( m_yLabel->c_str() );
    m_axisGraph->SetTitle( m_title.c_str() );
    m_axisGraph->Draw("AL");    

    // Draw all objects
    for( Stack::iterator o = m_objStack.begin(); o != m_objStack.end(); ++o) {
        (*o)->plotOn( this );
    }
    // Draw legend
    if( m_legend )
        m_legend->Draw();
    m_canvas->Update();
}

void Plot::save(const std::string& fname) {
    draw(true);
    m_canvas->SaveAs(fname.c_str(), "Landscape");
}

void Plot::pushObject(boost::shared_ptr<PlotObject> plot) {
    m_objStack.push_back( plot );
    // plot->plotOn(this);
}

void Plot::setLabel(Axis axis, const std::string& label) {
    switch( axis ) {
    case X: m_xLabel = label; break;
    case Y: m_yLabel = label; break;
    case Z: break;
    }
}

void Plot::setLogScale(Axis axis, bool l) {
    switch( axis ) {
    case X: m_xLog = l; break;
    case Y: m_yLog = l; break;
    case Z: m_zLog = l; break;
    }
}

void Plot::setLineColor(int color) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineColor(color);
}

void Plot::setFillColor(int color) {
    if( !m_objStack.empty() )
        m_objStack.back()->setFillColor(color);
}

void Plot::setLineWidth(int width) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineWidth(width);
}

void Plot::setLineStyle(Plot::LineStyle l) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineStyle( l );
}

void Plot::setMarkerStyle(Plot::MarkerStyle m) {
    if( !m_objStack.empty() )
        m_objStack.back()->setMarkerStyle( m );
}

void Plot::setHistText( bool txt ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistText( txt );
}

void Plot::setHistScatter( bool scat ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistScatter( scat );
}

void Plot::setHistContour( int n ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistContour( n );
}

void Plot::setHistColor( bool c ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistColor( c );
}

RangeM Plot::xRange() const {
    if( m_xRange.is_initialized() )
        return m_xRange;

    RangeM rng;
    for(Stack::const_iterator i = m_objStack.begin(); i != m_objStack.end(); ++i ) {
        rng = joinRange(rng, (*i)->xRange());
    }
    return rng;
}

RangeM Plot::yRange() const {
    if( m_yRange.is_initialized() )
        return m_yRange;

    RangeM rng;
    for(Stack::const_iterator i = m_objStack.begin(); i != m_objStack.end(); ++i ) {
        rng = joinRange(rng, (*i)->yRange());
    }
    return rng;
}

void Plot::setRange(Axis axis, double a, double b) {
    switch( axis ) {
    case X:
        m_xRange = Range(a,b);
        return;
    case Y:
        m_yRange = Range(a,b);
        return;
    case Z:
        // FIXME: treat Z!
        return;
    }
}

void Plot::setRange(Axis axis) {
    switch( axis ) {
    case X:
        m_xRange.reset();
        return;
    case Y:
        m_yRange.reset();
        return;
    case Z:
        // FIXME: treat Z!
        return;
    }
}

void Plot::removeLegend() {
    m_legend.reset();
}

void Plot::addLegend(double x1, double y1, double x2, double y2) {
    m_legend = boost::make_shared<TLegend>(x1, y1, x2, y2);
}

void Plot::addLegendString(const std::string& str) {
    if( m_legend )
        m_legend->AddEntry(new TObject(), str.c_str(), "");
}

void Plot::addPlotToLegend(const std::string& str) {
    if( m_legend && !m_objStack.empty() ) {
        TObject* obj = m_objStack.back()->getRootObject();
        if( obj )
            m_legend->AddEntry(obj, str.c_str() );
    }
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
    m_color    ( false )
{}

void PlotHist::plotOn(Plot*) {
    std::string opt = "SAME";
    if( m_text )
        opt += " TEXT";
    if( m_scatter )
        opt += " SCAT";
    if( m_box )
        opt += " BOX";
    if( m_color )
        opt += " COL";
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

RangeM PlotHist::xRange() const {
    return boost::optional<Range>(
        Range(
            hist->GetXaxis()->GetXmin(),
            hist->GetXaxis()->GetXmax() 
            ) );
}

RangeM PlotHist::yRange() const {
    TH2* h2d = dynamic_cast<TH2*>( &(*hist) );
    if( h2d ) {
        // 2D histogram
        return boost::optional<Range>(
            Range(
                hist->GetYaxis()->GetXmin(),
                hist->GetYaxis()->GetXmax() 
                ) );
    } else {
        // 1D histogram
        int    n     = hist->GetNbinsX();
        double yMax = 0;
        for( int i = 1; i <= n; i++ )
            yMax = std::max( yMax, hist->GetBinContent(i) );
        return boost::optional<Range>(
            Range( 0, 1.05 * yMax ) );
    }
}

TObject* PlotHist::getRootObject() {
    return &( *hist );
}


// ================================================================ //
// ==== Graph

PlotGraph::PlotGraph(TGraph* g) :
    color ( Plot::BLACK     ),
    line  ( Plot::SolidLine ),
    marker( Plot::NoMarker  ),
    graph ( g )
{}

void PlotGraph::plotOn(Plot*) {
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
    graph->Draw( opts.c_str() );
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

void PlotGraph::setLineColor(int col) {
    color = col;
    graph->SetLineColor(col);
}

RangeM PlotGraph::xRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* xs    = graph->GetX();
    double  hi    = *std::max_element(xs, xs+n);
    double  lo    = *std::min_element(xs, xs+n);
    double  delta = 0.03 * (hi - lo);
    return boost::optional<Range>( Range(lo - delta, hi + delta) );
}

RangeM PlotGraph::yRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* ys    = graph->GetY();
    double  hi    = *std::max_element(ys, ys+n);
    double  lo    = *std::min_element(ys, ys+n);
    double  delta = 0.03 * (hi - lo);
    return boost::optional<Range>( Range(lo - delta, hi + delta) );
}

TObject* PlotGraph::getRootObject() {
    return &( *graph );
}



// ================================================================ //
// ==== Poly

PlotPoly::PlotPoly(TPolyLine* g) :
    poly(g)
{
    setLineWidth( 0);
    setFillColor(20);
}

void PlotPoly::plotOn(Plot*) {
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

RangeM PlotPoly::xRange() const {
    int n = poly->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* xs    = poly->GetX();
    double  hi    = *std::max_element(xs, xs+n);
    double  lo    = *std::min_element(xs, xs+n);
    double  delta = 0.03 * (hi - lo);
    return boost::optional<Range>( Range(lo - delta, hi + delta) );
}

RangeM PlotPoly::yRange() const {
    int n = poly->GetN();
    if( n == 0 )
        return boost::optional<Range>();

    double* ys   = poly->GetY();
    double hi    = *std::max_element(ys, ys+n);
    double lo    = *std::min_element(ys, ys+n);
    double delta = 0.03 * (hi - lo);
    return boost::optional<Range>( Range(lo - delta, hi + delta) );
}

TObject* PlotPoly::getRootObject() {
    return &( *poly );
}



// ================================================================ //
// ==== Line

void PlotLine::plotOn(Plot* cxt) {
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

RangeM PlotLine::xRange() const {
    return boost::optional<Range>();
}

RangeM PlotLine::yRange() const {
    return boost::optional<Range>();
}



// ================================================================ //
// ==== Band


void PlotBand::plotOn(Plot* cxt) {
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
        double xs[4] = {x1,x2,x2,x1};
        double ys[4] = {lo,lo,hi,hi};
        poly = boost::shared_ptr<TPolyLine>( new TPolyLine(4, xs, ys, "") );
    }
    poly->SetFillColor( fill  );
    poly->SetFillStyle( 1001  );
    poly->SetLineWidth( width );
    poly->SetLineColor( color );
    poly->Draw("F");
    if( width > 0 )
        poly->Draw("");
}

void PlotBand::setFillColor(int col) {
    fill = col;
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
