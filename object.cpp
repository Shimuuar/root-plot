
#include "object.hpp"

#include <algorithm>

#include <TH1.h>
#include <TGraph.h>
#include <TCanvas.h>


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

void Plot::clear() {
    m_objStack.resize(0);
}

void Plot::draw(TCanvas* cnv) {
    // canvas->Clear();
    // FIXME!
}

void Plot::pushObject(PlotObject* plot) {
    m_objStack.push_back( boost::shared_ptr<PlotObject>(plot) );
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

void PlotGraph::plotOn(Plot*) {
    graph->Draw( "SAME" );
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
    return boost::optional<Range>( Range(hi,lo) );
}

RangeM PlotGraph::yRange() const {
    int n = graph->GetN();
    if( n == 0 )
        return boost::optional<Range>();
    
    double* ys = graph->GetY();
    double hi = *std::max_element(ys, ys+n);
    double lo = *std::min_element(ys, ys+n);
    return boost::optional<Range>( Range(hi,lo) );
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
