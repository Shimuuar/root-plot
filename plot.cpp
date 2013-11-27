
#include "object.hpp"

#include <assert.h>
#include <cmath>
#include <vector>
#include <fstream>
#include <iostream>
#include <boost/noncopyable.hpp>
#include <boost/make_shared.hpp>
#include <boost/format.hpp>

#include <TROOT.h>
#include <TCanvas.h>
#include <TPaveText.h>
#include <TColor.h>

#include "memory.hpp"



static bool endsWith(const std::string& str, const std::string& suf) {
    int n   = str.size();
    int len = suf.size();
    for( int i = 0; i < len; i++) {
        int off = n - i - 1;
        if( off < 0 || str[off] != suf[len - i - 1] )
            return false;
    }
    return true;
}


// ================================================================
// Layout tree

// Implementation of layout tree.
//
// List of valid states:
//
//  * Empty:      row == [] && plot == 0
//  * Row/Column: ∀ row     && plot == 0
//  * Pad:        row == [] && plot /= 0
//
struct Plot::Layout : boost::noncopyable {
public:
    // Possible states of layout
    enum State { EMPTY, PAD, ROW };

    // Data for subpads.
    struct PadData {
        PadData() :
            weight(1), pad(0)
        {}
        PadData(double w, Plot::Layout* l) :
            weight(w), pad(l)
        {}
        double        weight;
        Plot::Layout* pad;
    };

    // Create layout in empty state
    Layout(Layout* parent, TPad* pad);
    ~Layout();

    // Revert to empty state. After that
    void clear();
    // Draw everything
    void draw();
    // Adjust sizes of pads in the row
    void rebalanseRow();
    // Get tooltip
    bool getTooltip(int x, int y, std::string& str);
    // Check state
    bool isEmpty() { return state == EMPTY && plot == 0 && row.size() == 0; }
    bool isRow  () { return state == ROW   && plot == 0;                    }
    bool isPad  () { return state == PAD   && plot != 0 && row.size() == 0; }
    // Assert that structure is correct
    void assertValid();
    // Dump tree
    void dumpTree(int i = 0);


    Plot::Layout*        parent;      // Pointer to the parent structure. Null for root node
    TPad*                rootPad;     // ROOT's pad we operate on
    State                state;       // State of the layout
    Pad*                 plot;        // Pointer to the plot
    std::vector<PadData> row;         // Row data
    Plot::Orientation    orientation; // Orientation of row
};

Plot::Layout::Layout( Layout* p, TPad* rp ) :
    parent ( p  ),
    rootPad( rp ),
    state  ( EMPTY ),
    plot   ( 0     )
{
    assertValid();
}

Plot::Layout::~Layout() {
    clear();
    delete rootPad;
}

void Plot::Layout::clear() {
    // Clear row/column
    for( size_t i = 0; i < row.size(); i++ ) {
        delete row[i].pad;
    }
    row.resize(0);
    // Delete plot
    delete plot;
    plot = 0;
    // Set state
    state = EMPTY;
}

void Plot::Layout::draw() {
    assertValid();
    //
    if( isPad() ) {
        plot->draw();
    } else if( isRow() ) {
        for( size_t i = 0; i < row.size(); i++ ) {
            rootPad->cd();
            row[i].pad->rootPad->Draw();
            row[i].pad->draw();
        }
    }
}

void Plot::Layout::rebalanseRow() {
    // Total weight
    double sumW = 0;
    for( size_t i = 0; i < row.size(); i++ )
        sumW += row[i].weight;
    // Change sizes
    double off = 0;
    for( size_t i = 0; i < row.size(); i++ ) {
        double w = row[i].weight / sumW;
        switch( orientation ) {
        case Plot::Horizontal:
            row[i].pad->rootPad->SetPad( off,   0,
                                         off+w, 1 );
            break;
        case Plot::Vertical:
            row[i].pad->rootPad->SetPad( 0, 1 - off - w,
                                         1, 1 - off    );
            break;
        }
        off += w;
    }
}

bool Plot::Layout::getTooltip(int px, int py, std::string& res) {
    assertValid();
    //
    if( isEmpty() ) {
        return false;
    }
    if( isPad() ) {
        int px1 = rootPad->XtoAbsPixel( rootPad->GetX1() );
        int px2 = rootPad->XtoAbsPixel( rootPad->GetX2() );
        int py1 = rootPad->YtoAbsPixel( rootPad->GetY1() );
        int py2 = rootPad->YtoAbsPixel( rootPad->GetY2() );

        bool inside =  (px1 <= px) && (px2 >= px)
                    && (py1 >= py) && (py2 <= py);

        if( inside ) {
            double x = rootPad->AbsPixeltoX( px );
            double y = rootPad->AbsPixeltoY( py );
            // Check for log scale
            if( rootPad->GetLogx() )
                x = exp10(x);
            if( rootPad->GetLogy() )
                y = exp10(y);
            // Create tooltip
            res = (boost::format( " X = %.3g\n Y = %.3g" ) % x % y).str();
            return true;
        }
        return false;
    }
    if( isRow() ) {
        for( size_t  i = 0; i < row.size(); i++) {
            if( row[i].pad->getTooltip(px,py,res) )
                return true;
        }
        return false;
    }
    //
    assert(false && "Unreachable");
    return false;
}

void Plot::Layout::assertValid() {
    assert( "Layout state is valid" &&
            rootPad != 0            &&
            ( isEmpty() || isPad() || isRow() ) );
}

void Plot::Layout::dumpTree(int n) {
    std::string off( n, ' ');
    std::cout << off << (void*)this
              << " [ " << (void*)(parent) << " ] "
              << (orientation == Plot::Horizontal ? "H" : "V")
              << std::endl;
    for( size_t  i = 0; i < row.size(); i++) {
        row[i].pad->dumpTree(n + 4);
    }
}



// ================================================================

Plot::Plot( TCanvas* cnv ) :
    m_silent ( false ),
    m_canvas ( cnv   )
{
    doSetPalette( DeepSea );
    // Layouts are initialized here
    clear();
}

std::string Plot::getTooltip(int x, int y) {
    std::string res;
    m_layout->getTooltip(x,y,res);
    return res;
}

void Plot::setCanvasSize(int x, int y) {
    m_xSize = x;
    m_ySize = y;
}

void Plot::fatalError(const std::string& str ) {
    m_current = 0;
    m_errors.push_back( str );
}

void Plot::pushCommand(const std::string& str ) {
    m_commands.push_back( str );
}

void Plot::draw(bool force) {
    if( !m_silent || (m_silent && force) ) {
        m_canvas->cd();
        // Resize canvas if needed
        if( m_xSize > 0 && m_ySize > 0 ) {
            m_canvas->SetCanvasSize( m_xSize, m_ySize );
        } else {
            m_canvas->Resize();
        }
        // Draw
        m_layout->rootPad->Draw();
        m_layout->draw();
        m_canvas->Update();
    }
    if( m_errors.size() > 0 ) {
        // [re]create pad for reporting errors
        m_canvas->cd();
        m_errorPad  = makeROOT<TPad>( "Error PAD", "Errors", 0.1, 0.5, 0.9, 0.9 );
        m_errorPad->SetFillColor( 4 );
        m_errorPad->cd();
        // Draw list of errors
        TPaveText* errorText = new TPaveText( 0, 0, 1, 1 );
        for( size_t  i = 0; i < m_errors.size(); i++) {
            errorText->AddText( m_errors[i].c_str() );
        }
        errorText->Draw();
        // Draw error pad
        m_canvas->cd();
        m_errorPad->Draw();
        m_errorPad->Update();
    }
}

void Plot::save(const std::string& fname) {
    // Check that filename is valid
    for(std::string::const_iterator i = fname.begin(); i != fname.end(); ++i) {
        char c = *i;
        if( !( isalnum(c) || c=='-' || c=='.' || c=='_' || c=='=' || c=='+'
               || c=='(' || c==')' || c==':' || c==',') )
            return;
        std::cerr << "Cannot save to file: `" << fname << "'\n";
    }
    // We are saving script
    if( endsWith( fname, ".rootpl" ) ) {
        std::ofstream f( fname.c_str() );
        for( std::list<std::string>::iterator i = m_commands.begin(); i != m_commands.end(); ++i ) {
            f << *i << std::endl;
        }
        return;
    }
    // Force plot update
    draw( true );
    m_canvas->SaveAs(fname.c_str(), "Landscape");
}

void Plot::reportError(const std::string& str) {
    m_errors.push_back( str );
}

void Plot::clear() {
    // Clear commands list
    m_commands.clear();
    // Reset silent mode
    m_silent = false;
    // Remove errors
    m_errors.resize( 0 );
    m_errorPad.reset();
    // Reset canvas size
    m_xSize = -1;
    m_ySize = -1;
    // Delete all plots
    m_canvas->cd();
    m_layout  = boost::make_shared<Layout>( (Layout*)0, newROOT<TPad>("PAD","", 0,0, 1,1) );
    m_current = &*m_layout;
    // Delete extra canvases. They could appear when one creates slice
    TIter next( dynamic_cast<TList*>( gROOT->GetListOfCanvases() ) );
    for(TCanvas *cnv; (cnv = dynamic_cast<TCanvas*>(next())); ) {
        if( cnv != m_canvas )
            delete cnv;
    }
}

void Plot::setSilent(bool s) {
    m_silent = s;
}

void Plot::addRow( Plot::Orientation o, double w ) {
    if( !m_current )
        return;
    m_current->assertValid();
    //
    if( m_current->isEmpty() ) {
        m_current->state       = Layout::ROW;
        m_current->orientation = o;
        m_current->assertValid();
    } else if( m_current->isRow() ) {
        addPad( w );
        addRow( o );
    } else {
        fatalError("Cannot add new row");
    }
}

void Plot::completeRow() {
    if( !m_current )
        return;
    m_current->assertValid();
    //
    if( !m_current->isRow() ) {
        fatalError( "Cannot end row/column." );
    } else {
        // If top level pad is row/column and we ended it. There is
        // nothing to be done after it. We silently enter invalid
        // state
        m_current = m_current->parent;
        if( m_current )
            m_current->assertValid();
    }
}

void Plot::addPad( double weight ) {
    if( !m_current )
        return;
    m_current->assertValid();
    //
    if( m_current->isRow() ) {
        m_current->rootPad->cd();
        // Add new pad
        TPad*   pad   = newROOT<TPad>( "PAD", "", 0, 0, 1, 1);
        Layout* child = new Layout( m_current, pad  );
        m_current->row.push_back( Layout::PadData( weight, child) );
        m_current->rebalanseRow();
        m_current = child;
    } else {
        fatalError( "Cannot add new pad - not in row mode" );
    }
}

void Plot::completePad() {
    if( !m_current )
        return;
    m_current->assertValid();
    //
    if( ! (m_current->isEmpty() || m_current->isPad()) ) {
        fatalError( "Cannot complete pad - not in pad mode" );
    } else if( m_current->parent == 0 ) {
        fatalError( "Cannot complete pad - at top level" );
    } else {
        m_current = m_current->parent;
        m_current->assertValid();
    }
}

Pad* Plot::getCurrentPlot() {
    if( !m_current )
        return 0;
    m_current->assertValid();
    //
    if( m_current->isEmpty() ) {
        m_current->plot  = new Pad( m_current->rootPad );
        m_current->state = Layout::PAD;
        return m_current->plot;
    } else if( m_current->isPad() ) {
        return m_current->plot;
    } else {
        fatalError( "Cannot draw - not in drawing mode" );
        return 0;
    }
}

void Plot::pushObject( boost::shared_ptr<PlotObject> o ) {
    Pad* pad = getCurrentPlot();
    if( pad )
        pad->pushObject(o);
}

void Plot::setPalette(Palette palette) {
    if( palette != m_palette )
        doSetPalette( palette );
}

void Plot::doSetPalette(Palette palette) {
    m_palette = palette;
    switch( m_palette ) {
    case DeepSea:
    {
        TColor::InitializeColors();
        const UInt_t nRGBs = 5;
        Double_t stops[nRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
        Double_t red[nRGBs]   = { 0.00, 0.09, 0.18, 0.09, 0.00 };
        Double_t green[nRGBs] = { 0.01, 0.02, 0.39, 0.68, 0.97 };
        Double_t blue[nRGBs]  = { 0.17, 0.39, 0.62, 0.79, 0.97 };
        TColor::CreateGradientColorTable(nRGBs, stops, red, green, blue, 255);
        break;
    }
    case GreyScale:
    {
        TColor::InitializeColors();
        const Int_t nRGBs = 3;
        Double_t stops[nRGBs] = { 0.00, 0.50, 1.00};
        Double_t red[nRGBs]   = { 0.00, 0.50, 1.00};
        Double_t green[nRGBs] = { 0.00, 0.50, 1.00};
        Double_t blue[nRGBs]  = { 0.00, 0.50, 1.00};
        TColor::CreateGradientColorTable(nRGBs, stops, red, green, blue, 255);
        break;
    }
    case BlackBody:
    {
        TColor::InitializeColors();
        const Int_t nRGBs = 5;
        Double_t stops[nRGBs] = { 0.00, 0.25, 0.50, 0.75, 1.00};
        Double_t red[nRGBs]   = { 0.00, 0.50, 1.00, 1.00, 1.00};
        Double_t green[nRGBs] = { 0.00, 0.00, 0.55, 1.00, 1.00};
        Double_t blue[nRGBs]  = { 0.00, 0.00, 0.00, 0.00, 1.00};
        TColor::CreateGradientColorTable(nRGBs, stops, red, green, blue, 255);
        break;
    }
    case BlueYellow:
    {
        TColor::InitializeColors();
        const Int_t nRGBs = 3;
        Double_t stops[nRGBs] = { 0.00, 0.50, 1.00};
        Double_t red[nRGBs]   = { 0.00, 0.50, 1.00};
        Double_t green[nRGBs] = { 0.00, 0.50, 1.00};
        Double_t blue[nRGBs]  = { 0.50, 0.50, 0.00};
        TColor::CreateGradientColorTable(nRGBs, stops, red, green, blue, 255);
        break;
    }
    }
}
