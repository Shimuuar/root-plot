
#include "object.hpp"

#include <algorithm>

#include <TH1.h>
#include <TGraph.h>
#include <TCanvas.h>



// ================================================================ //
// ==== Plot

void Plot::clear() {
    canvas->Clear();
    objStack.resize(0);
}

void Plot::draw() {
    canvas->Clear();
}

void Plot::redraw() {
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
