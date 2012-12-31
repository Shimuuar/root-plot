
#include "object.hpp"

#include <assert.h>
#include <vector>
#include <boost/noncopyable.hpp>
#include <boost/make_shared.hpp>

#include <TCanvas.h>
#include <TPaveText.h>



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
// Note on ownership of ROOT objects. Layout do not own TPad. Instead
// they are owned by the parent. This is because root TCanvas
// shouldn't be deleted.
struct Plot::Layout : boost::noncopyable {
public:
    enum State { EMPTY, PAD, ROW };
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
    // Check state
    bool isEmpty() { return state == EMPTY && plot == 0 && row.size() == 0; }
    bool isRow  () { return state == ROW   && plot == 0;                    }
    bool isPad  () { return state == PAD   && plot != 0 && row.size() == 0; }
    // Assert that structure is correct
    void assertValid();
    // Dump tree
    void dumpTree(int i = 0);

    // Pointer to the parent structure. Null for root node
    Plot::Layout*     parent;
    // ROOT's pad we operate on
    TPad*             rootPad;

    // Payload. Very
    State                state;       // State of
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
}

void Plot::Layout::clear() {
    // Clear row/column
    for( size_t i = 0; i < row.size(); i++ ) {
        delete row[i].pad->rootPad;
        row[i].pad->rootPad = 0;
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

void Plot::Layout::assertValid() {
    assert( "Layout state is valid" &&
            rootPad != 0            &&
            ( isEmpty() || isPad() || isRow() ) );
}

// ================================================================

Plot::Plot( TCanvas* cnv ) :
    m_silent ( false ),
    m_canvas ( cnv   ),
    m_layout ( new Layout( 0, cnv ) ),
    m_current( m_layout )
{}

void Plot::fatalError(const std::string& str ) {
    m_current = 0;
    m_errors.push_back( str );
}

void Plot::draw() {
    if( !m_silent ) {
        m_layout->draw();
        m_canvas->cd();
        m_canvas->Draw();
        m_canvas->Update();
    }
    if( m_errors.size() > 0 ) {
        // [re]create pad for reporting errors
        m_canvas->cd();
        m_errorPad  = boost::make_shared<TPad>( "Error PAD", "Errors", 0.1, 0.5, 0.9, 0.9 );
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

void Plot::reportError(const std::string& str) {
    m_errors.push_back( str );
}

void Plot::clear() {
    m_silent = false;
    m_errors.resize( 0 );
    m_errorPad.reset();

    m_layout->clear();
    m_current = m_layout;
}

void Plot::setSilent(bool s) {
    m_silent = s;
}

void Plot::addRow( Plot::Orientation o ) {
    if( !m_current )
        return;
    m_current->assertValid();
    //
    if( m_current->isEmpty() ) {
        m_current->state       = Layout::ROW;
        m_current->orientation = o;
        m_current->assertValid();
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
        Layout* child = new Layout( m_current, new TPad( "PAD", "", 0, 0, 1, 1) );
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
