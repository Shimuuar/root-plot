
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
// ==== Legend

struct Pad::Legend {
    // Entry for the legend
    struct Entry {
        Entry() :
            idx(-1)
        {}
        Entry(const std::string& s) :
            idx(-1), str1(s)
        {}
        Entry(const std::string& s1, const std::string& s2) :
            idx(-1), str1(s1), str2(s2)
        {}
        Entry(int i, const std::string& s) :
            idx(i), str1(s)
        {}
        // ----------------------------------------
        int         idx;        // Index in the vector of objects.
        std::string str1;       // String
        std::string str2;       // Second string
    };

    Legend();
    // Remove everything from legend
    void clear();
    // Set coordinates
    void setCoords(double x1, double y1, double x2, double y2);
    // Add entry to legend
    void addEntry(const Entry& e);
    // Create nice legend
    boost::shared_ptr<TPave> makeLegend(const Pad::Stack&);

    bool               isInit;      // Is legend initialized
    double             x1,x2,y1,y2; // Coordinates for legend/text box
    std::vector<Entry> entries;     // Legend entries
};

Pad::Legend::Legend() {
    clear();
}

void Pad::Legend::clear() {
    isInit = false;
    entries.resize( 0 );
}

void Pad::Legend::addEntry(const Entry& e) {
    entries.push_back( e );
}

void Pad::Legend::setCoords(double x1_, double y1_, double x2_, double y2_) {
    isInit = true;
    x1 = x1_;
    x2 = x2_;
    y1 = y1_;
    y2 = y2_;
}

boost::shared_ptr<TPave> Pad::Legend::makeLegend(const Pad::Stack& objs) {
    if( !isInit )
        return boost::shared_ptr<TPave>();
    // Allocate TPave
    boost::shared_ptr<TLegend> pave;
    // For now we use 
    pave = boost::make_shared<TLegend>(x1, y1, x2, y2);
    for( size_t i = 0; i < entries.size(); i++) {
        Entry& e = entries[i];
        if( e.idx < 0 ) {
            // Add entry without plot sample
            pave->AddEntry( (TObject*)0, e.str1.c_str(), "");
        } else {
            assert( e.idx < (int)objs.size() && "Index must be in range" );
            pave->AddEntry( objs[e.idx]->getRootObject(),
                            e.str1.c_str() );
        }
    }
    return pave;
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
    m_legend( new Legend )
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
    m_rootLegend = m_legend->makeLegend( m_objStack );
    if( m_rootLegend )
        m_rootLegend->Draw();
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
    m_legend->clear();
    m_rootLegend.reset();
}

void Pad::addLegend(double x1, double y1, double x2, double y2) {
    m_legend->setCoords(x1,y1, x2,y2);
}

void Pad::addLegendString(const std::string& str) {
    m_legend->addEntry( Legend::Entry( str ) );
}

void Pad::addPlotToLegend(const std::string& str) {
    if( !m_objStack.empty() ) {
        m_legend->addEntry( Legend::Entry( m_objStack.size() - 1, str ) );
    }
}
