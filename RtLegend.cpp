
#include "RtLegend.hpp"

#include <cmath>
#include <boost/make_shared.hpp>

#include <TROOT.h>
#include <TLatex.h>

#include <TPave.h>
#include <TStyle.h>
#include <TVirtualPad.h>
#include <TClass.h>

#include "object.hpp"


class RtLegend::Entry {
public:
    Entry() {}
    Entry(const std::string& s) :
        str1(s)
    {}
    Entry(const std::string& s1, const std::string& s2) :
        str1(s1), str2(s2)
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
    std::cout << "RtLegend::Copy\n";
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

    // At first we need to determine font size for drawing
    double nRows    = entries.size();
    double fontSize = (1 - c_rowPadding) / nRows;
    // Find out maximum width of text
    double maxX1 = 0;
    double maxX2 = 0;
    for( size_t i = 0; i < entries.size(); i++) {
        Entry& e = *entries[i];
        {
            TLatex latex(0,0, e.str1.c_str() );
            latex.SetTextSize( fontSize );
            maxX1 = std::max( latex.GetXsize(), maxX1 );
        }
        {
            TLatex latex(0,0, e.str2.c_str() );
            latex.SetTextSize( fontSize );
            maxX2 = std::max( latex.GetXsize(), maxX2 );
        }

    }
    // We may have one or two columns. Space requirements are
    // different for both cases
    if( maxX2 > 0 ) {
        if( (maxX1 + maxX2) > (1 - 3*c_colPadding) )
            fontSize *= (1- 3*c_colPadding) / (maxX1 + maxX2);
    } else {
        if( maxX1 > (1 - 2*c_colPadding) )
            fontSize *= (1- 2*c_colPadding) / maxX1;
    }
    // Then we need to check whether we need to draw plot snippets
    //
    // FIXME:

    for( size_t i = 0; i < entries.size(); i++) {
        Entry& e = *entries[i];
        {
            double x = c_colPadding;
            double y = (nRows - i - 0.5) / nRows;
            TLatex latex( drawX(x), drawY(y), e.str1.c_str() );
            latex.SetTextAlign( 12 ); // left-centered
            latex.SetTextSize( fontSize );
            latex.Paint();
        }
        if( !e.str2.empty() ) {
            double x = 1 - c_colPadding;
            double y = (nRows - i - 0.5) / nRows;
            TLatex latex( drawX(x), drawY(y), e.str2.c_str() );
            latex.SetTextAlign( 32 ); // right-centered
            latex.SetTextSize( fontSize );
            latex.Paint();
        }
    }
}
