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
// ==== Helper functions for range

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
    // Select ranges and padding
    Range rng;
    // Low range
    if( r1->low < r2->low ) {
        rng.low        = r1->low;
        rng.wantPadLow = r1->wantPadLow;
    } else {
        rng.low        = r2->low;
        rng.wantPadLow = r2->wantPadLow;
    }
    // High range
    if( r1->hi > r2->hi ) {
        rng.hi        = r1->hi;
        rng.wantPadHi = r1->wantPadHi;
    } else {
        rng.hi        = r2->hi;
        rng.wantPadHi = r2->wantPadHi;
    }
    // Log
    rng.logLow = joinLogLow( r1->logLow, r2->logLow );
    return rng;
}

double Range::lowRange(bool useLog) {
    if( useLog ) {
        // Find out minimum for log scale. logLow is used if it's set.
        double l = logLow.get_value_or( low );
        // Otherwise we may need to apply padding manually.
        if( wantPadLow )
            return exp( log(l) - 0.03 * log(hi/l));
        else
            return l;
    } else {
        // In linear scale we simply add padding when appropriate.
        if( wantPadLow )
            return low - 0.03 * (hi - low);
        else
            return low;
    }
}

double Range::hiRange(bool useLog) {
    if( useLog ) {
        // Apply padding in log scale
        double l = logLow.get_value_or( low );
        if( wantPadHi )
            return exp( log(hi) + 0.03 * log(hi/l));
        else
            return hi;
    } else {
        // Apply padding in linear scale
        if( wantPadHi )
            return hi + 0.03 * (hi - low);
        else
            return hi;
    }
}

// ================================================================ //
// ==== Pad

Pad::Pad(TPad* cnv) :
    m_canvas(cnv),
    m_gridX( false ),
    m_gridY( false ),
    m_xLog( false ),
    m_yLog( false ),
    m_zLog( false ),
    m_rangeDirty( false )
{
    m_canvas->cd();
}

Pad::~Pad()
{}

// Remove everything from canvas
void Pad::clearCanvas() {
    m_canvas->Clear();
    m_canvas->cd();
    m_canvas->SetLogx( m_xLog );
    m_canvas->SetLogy( m_yLog );
    m_canvas->SetLogz( m_zLog );
}

void Pad::clear() {
    m_objStack.resize(0);
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

    // Set ranges for graph and clean dirty flag. It will reset at the
    // end of function
    recalculateRange();
    m_rangeDirty = false;

    // Draw frame for the plots.
    TH1* hist = m_canvas->DrawFrame(
        m_xRange[0], m_yRange[0],
        m_xRange[1], m_yRange[1]);
    if( !!m_xLabel )
        hist->GetXaxis()->SetTitle( m_xLabel->c_str() );
    if( !!m_yLabel )
        hist->GetYaxis()->SetTitle( m_yLabel->c_str() );
    if( m_title.size() > 0 )
        hist->SetTitle( m_title.c_str() );

    // Draw all objects
    for( Stack::iterator o = m_objStack.begin(); o != m_objStack.end(); ++o) {
        (*o)->plotOn( this );
    }

    // Draw legend
    if( m_legend ) {
        // Create and draw pad for legend
        m_legendPad = makeROOT<TPad>("LP","", m_legX1, m_legY1,  m_legX2, m_legY2 );
        m_legendPad->Draw();
        // Draw legend
        m_legendPad->cd();
        m_legend->Draw();
        m_canvas->cd();
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

void Pad::setHistTextFmt( int n ) {
    if( !m_objStack.empty() )
        m_objStack.back()->setHistTextFmt( n );
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


// Function which determines range for given axis
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
        if( (*i)->isAutorange ) {
            switch( axis ) {
            case Plot::X: rng = joinRange(rng, (*i)->xRange()); break;
            case Plot::Y: rng = joinRange(rng, (*i)->yRange()); break;
            case Plot::Z: rng = joinRange(rng, (*i)->zRange()); break;
            }
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

void Pad::recalculateRange() {
    m_xRange[0] = m_xLog ? 0.01 : 0;
    m_xRange[1] = 1;
    m_yRange[0] = m_yLog ? 0.01 : 0;
    m_yRange[1] = 1;

    RangeM rngX = axisRange( m_xLow, m_xHi, m_objStack, Plot::X );
    if( rngX.is_initialized() ) {
        m_xRange[0] = rngX->lowRange( m_xLog );
        m_xRange[1] = rngX->hiRange ( m_xLog );
    }
    RangeM rngY = axisRange( m_yLow, m_yHi, m_objStack, Plot::Y );
    if( rngY.is_initialized() ) {
        m_yRange[0] = rngY->lowRange( m_yLog );
        m_yRange[1] = rngY->hiRange ( m_yLog );
    }
}

std::pair<double,double> Pad::xRange() {
    if( m_rangeDirty )
        recalculateRange();
    return std::make_pair( m_xRange[0], m_xRange[1] );
}

std::pair<double,double> Pad::yRange() {
    if( m_rangeDirty )
        recalculateRange();
    return std::make_pair( m_yRange[0], m_yRange[1] );
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

void Pad::setAutoRange(bool flag) {
    if( !m_objStack.empty() )
        m_objStack.back()->isAutorange = flag;
}

void Pad::removeLegend() {
    m_legend.reset();
    m_legendPad.reset();
}

void Pad::addLegend(double x1, double y1, double x2, double y2) {
    m_legX1  = x1;
    m_legX2  = x2;
    m_legY1  = y1;
    m_legY2  = y2;
    m_legend = makeROOT<RtLegend>(0,0, 1,1);
}

void Pad::addLegendString(const std::string& str) {
    if( m_legend )
        m_legend->addEntry( str );
}

void Pad::addLegendString(const std::string& key, const std::string& val) {
    if( m_legend )
        m_legend->addEntry( key, val );
}

void Pad::addPlotToLegend(const std::string& str) {
    if( !m_objStack.empty() && m_legend ) {
        m_legend->addEntry( m_objStack.back(), str );
    }
}
