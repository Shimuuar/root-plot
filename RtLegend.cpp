
#include "RtLegend.hpp"

#include <assert.h>
#include <cmath>
#include <boost/make_shared.hpp>

#include <TROOT.h>
#include <TLatex.h>

#include <TPave.h>
#include <TStyle.h>
#include <TVirtualPad.h>
#include <TClass.h>
#include <TMarker.h>

#include "object.hpp"


class RtLegend::Entry {
public:
    Entry() {}
    Entry(const std::string& s) :
        obj( (PlotObject*)0 ),str1(s)
    {}
    Entry(const std::string& s1, const std::string& s2) :
        obj( (PlotObject*)0 ),str1(s1), str2(s2)
    {}
    Entry(boost::shared_ptr<PlotObject> o, const std::string& s) :
        obj(o), str1(s)
    {}

    boost::shared_ptr<PlotObject> obj;  // Object for legend
    std::string                   str1; // String
    std::string                   str2; // Second string (optional)
};

// ================================================================
// Legend

RtLegend::RtLegend(double x1, double y1, double x2, double y2) :
    TPave(x1,y1,x2,y2),
    // We store coordinates for later use
    m_X1( x1 ), m_X2( x2 ),
    m_Y1( y1 ), m_Y2( y2 )
{}

RtLegend::~RtLegend()
{}

void RtLegend::addEntry(boost::shared_ptr<PlotObject> o, const std::string& str) {
    entries.push_back( boost::make_shared<Entry>( o, str ) );
}

void RtLegend::addEntry(const std::string& str) {
    entries.push_back( boost::make_shared<Entry>( str ) );
}

void RtLegend::addEntry(const std::string& key, const std::string& val) {
    entries.push_back( boost::make_shared<Entry>( key, val ) );
}

double RtLegend::drawX(double x, bool forceLinear) {
    double b    = gPad->PixeltoX( GetBorderSize() );
    double xp1  = gPad->GetX1();
    double dpx  = (gPad->GetX2() - xp1) * (1 - b);

    x = xp1 + dpx*(m_X1 + (m_X2 - m_X1)*x);
    if( forceLinear || !gPad->GetLogx() )
        return x;
    else
        return exp10( x );

}

double RtLegend::drawY(double y, bool forceLinear) {
    double b    = fabs( gPad->PixeltoY( GetBorderSize() ) );
    double yp1  = gPad->GetY1();
    double dpy  = gPad->GetY2() - yp1;
    yp1 += b * dpy;
    dpy *= (1 - b);

    y = yp1 + dpy*(m_Y1 + (m_Y2 - m_Y1)*y);
    if( forceLinear || !gPad->GetLogy() )
        return y;
    else
        return exp10( y );
}

// ================================================================

void RtLegend::Copy(TObject& o) {
    assert(false && "Unimplemented");
}

void RtLegend::Clear(Option_t* opt) {
    // FIXME
}

void RtLegend::Draw(Option_t* opt) {
    AppendPad(opt);
}

void RtLegend::Paint(Option_t* opt) {
    // Draw pad
    SetFillColor( kWhite );
    TPave::PaintPave( drawX(0,true), drawY(0,true),
                      drawX(1,true), drawY(1,true),
                      GetBorderSize(), opt);

    // Layout constants
    const double c_rowPadding = 0.1;
    const double c_colPadding = 0.03;
    const double c_snippet    = 0.2;

    // ----------------------------------------------------------------
    // First we need to determine whether we need to draw plot
    // snippets and how many columns do we have
    bool haveSnippets = false;
    int  nCols        = 1;
    for( size_t i = 0; i < entries.size(); i++ ) {
        if( entries[i]->obj )
            haveSnippets = true;
        if( !entries[i]->str2.empty() )
            nCols = 2;
    }

    // ----------------------------------------------------------------
    // Now we need to adjust font size. We start from finding out size
    // of box into which label must fit.
    double nRows    = entries.size();
    double height   = (1 - c_rowPadding) / nRows;
    double width    = 1;
    if( haveSnippets )
        width -= c_snippet;
    if( nCols == 1 )
        width -= 2 * c_colPadding;
    else
        width -= 3 * c_colPadding;
    // Now font size
    double fontSize = height;
    double maxX1 = 0, maxY1 = 0;
    double maxX2 = 0, maxY2 = 0;
    for( size_t i = 0; i < entries.size(); i++) {
        Entry& e = *entries[i];
        {
            TLatex latex(0,0, e.str1.c_str() );
            latex.SetTextSize( fontSize );
            maxX1 = std::max( latex.GetXsize(), maxX1 );
            maxY1 = std::max( latex.GetYsize(), maxY1 );
        }
        {
            TLatex latex(0,0, e.str2.c_str() );
            latex.SetTextSize( fontSize );
            maxX2 = std::max( latex.GetXsize(), maxX2 );
            maxY2 = std::max( latex.GetYsize(), maxY2 );
        }

    }
    // Finally adjust font size
    double overflowRate = std::max( (maxX1 + maxX2) / width,
                                    std::max(maxY1,maxY2) / height );
    if( overflowRate > 1 )
        fontSize /= overflowRate;

    // ----------------------------------------------------------------
    // Actual drawing
    for( size_t i = 0; i < entries.size(); i++) {
        Entry& e = *entries[i];
        double y = (nRows - i - 0.5) / nRows;
        // First text entry
        {
            double x = c_colPadding + (haveSnippets ? c_snippet : 0);
            paintText( x, y, e.str1, fontSize, 12 );
        }
        if( !e.str2.empty() ) {
            double x = 1 - c_colPadding;
            paintText( x, y, e.str2, fontSize, 32 );
        }
        // Now we draw plot snippets
        TObject* o;
        if( e.obj && (o = e.obj->getRootObject()) ) {
            double x0 = drawX( c_snippet / 2            );
            double x1 = drawX( c_colPadding             );
            double x2 = drawX( c_snippet - c_colPadding );

            double y0 = drawY( y );
            double y1 = drawY( y - 0.4*height );
            double y2 = drawY( y + 0.4*height );

            TAttFill*   fill   = dynamic_cast<TAttFill*>  ( o );
            TAttLine*   line   = dynamic_cast<TAttLine*>  ( o );
            TAttMarker* mStyle = dynamic_cast<TAttMarker*>( o );

            // Draw fill area if needed
            if( fill && e.obj->haveFill() ) {
                double xs[] = {x1,x1,x2,x2};
                double ys[] = {y1,y2,y2,y1};
                fill->TAttFill::Modify();
                gPad->PaintFillArea(4, xs, ys);
            }
            // Draw line
            if( line ) {
                line->TAttLine::Modify();
                gPad->PaintLineNDC( x1, y0, x2, y0 );
            }
            // Draw marker
            if( mStyle ) {
                TMarker marker(x0,y0, 0);
                marker.SetNDC();
                mStyle->TAttMarker::Copy( marker );
                marker.Paint();
            }
        }
    }
}

void RtLegend::paintText(double x, double y, const std::string& str,
                         double fontSize, int align)
{
    TLatex latex( drawX(x), drawY(y), str.c_str() );
    latex.SetTextSize ( fontSize );
    latex.SetTextAlign( align    );
    latex.Paint();
}
