
#include "object.hpp"

#include <algorithm>
#include <boost/make_shared.hpp>

#include <TH1.h>
#include <TGraph.h>
#include <TCanvas.h>
#include <TROOT.h>



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
}

void Plot::clear() {
    m_objStack.resize(0);
    clearCanvas();
}

void Plot::draw() {
    if( m_isSilent )
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
    m_axisGraph->Draw("AL");    
    
    for( Stack::iterator o = m_objStack.begin(); o != m_objStack.end(); ++o) {
        (*o)->plotOn( this );
    }
    m_canvas->Update();
}

void Plot::pushObject(boost::shared_ptr<PlotObject> plot) {
    m_objStack.push_back( plot );
    // plot->plotOn(this);
}

void Plot::setLineColor(Plot::Color color) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineColor(color);
}

void Plot::setLineWidth(int width) {
    if( !m_objStack.empty() )
        m_objStack.back()->setLineWidth(width);
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



// ================================================================ //
// ==== Histogram
// ================================================================ //

PlotHist::PlotHist(TH1* h) :
    hist(h)
{}

void PlotHist::plotOn(Plot*) {
    hist->Draw( "SAME" );
}

void PlotHist::setLineWidth(int width) {
    hist->SetLineWidth(width);
}

void PlotHist::setLineColor(Plot::Color col) {
    hist->SetLineColor(col);
}

RangeM PlotHist::xRange() const {
    // FIXME!
    return boost::optional<Range>();
}

RangeM PlotHist::yRange() const {
    // FIXME!
    return boost::optional<Range>();
}



// ================================================================ //
// ==== Graph

PlotGraph::PlotGraph(TGraph* g) :
    graph(g)
{}

void PlotGraph::plotOn(Plot*) {
    std::string opts = "L SAME";
    graph->Draw( opts.c_str() );
}

void PlotGraph::setLineWidth(int width) {
    graph->SetLineWidth(width);
}

void PlotGraph::setLineColor(Plot::Color col) {
    graph->SetLineColor(col);
}

RangeM PlotGraph::xRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    
    double* xs = graph->GetX();
    double hi = *std::max_element(xs, xs+n);
    double lo = *std::min_element(xs, xs+n);
    double delta = 0.03 * (hi - lo);
    return boost::optional<Range>( Range(lo - delta, hi + delta) );
}

RangeM PlotGraph::yRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    
    double* ys = graph->GetY();
    double hi = *std::max_element(ys, ys+n);
    double lo = *std::min_element(ys, ys+n);
    double delta = 0.03 * (hi - lo);
    return boost::optional<Range>( Range(lo - delta, hi + delta) );
}



// ================================================================ //
// ==== Line


void PlotLine::plotOn(Plot* cxt) {
    // FIXME
}

void PlotLine::setLineWidth(int w) {
    width = w;
}

void PlotLine::setLineColor(Plot::Color col) {
    color = col;
}

RangeM PlotLine::xRange() const {
    return boost::optional<Range>();
}

RangeM PlotLine::yRange() const {
    return boost::optional<Range>();
}
