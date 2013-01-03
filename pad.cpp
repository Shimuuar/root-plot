
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
    m_gridX( false ),
    m_gridY( false ),
    m_xLog( false ),
    m_yLog( false ),
    m_zLog( false )
{
    m_canvas->cd();
}

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