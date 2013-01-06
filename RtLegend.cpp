
#include "RtLegend.hpp"

#include <boost/make_shared.hpp>

#include <TROOT.h>
#include <TLatex.h>

#include "TPave.h"
#include "TStyle.h"
#include "TVirtualPad.h"
#include "TClass.h"
#include "TMath.h"


class RtLegend::Entry {
public:
    Entry() {}
    Entry(const std::string& s) :
        str1(s)
    {}
    Entry(const std::string& s1, const std::string& s2) :
        str1(s1), str2(s2)
    {}
    Entry(boost::shared_ptr<TObject> o, const std::string& s) :
        obj(o), str1(s)
    {}

    boost::shared_ptr<TObject> obj;  // Object for legend
    std::string                str1; // String
    std::string                str2; // Second string (optional)
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

void RtLegend::addEntry(boost::shared_ptr<TObject> o, const std::string& str) {
    entries.push_back( boost::make_shared<Entry>( o, str ) );
}

void RtLegend::addEntry(const std::string& str) {
    entries.push_back( boost::make_shared<Entry>( str ) );
}

void RtLegend::addEntry(const std::string& key, const std::string& val) {
    entries.push_back( boost::make_shared<Entry>( key, val ) );
}

double RtLegend::drawX(double x) {
    double xp1  = gPad->GetX1();
    double dpx  = gPad->GetX2() - xp1;

    x = xp1 + dpx * (m_X1 + (m_X2 - m_X1)*x);
    if( gPad->GetLogx() )
        return exp10( x );
    else
        return x;
}

double RtLegend::drawY(double y) {
    double yp1  = gPad->GetY1();
    double dpy  = gPad->GetY2() - yp1;

    y = yp1 + dpy * (m_Y1 + (m_Y2 - m_Y1)*y);
    if( gPad->GetLogy() )
        return exp10( y );
    else
        return y;
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
    // For some reason coordinate conversion provide by ROOT doesn't
    // work. I had to perform conversion manually.
    {
        Double_t xp1  = gPad->GetX1();
        Double_t dpx  = gPad->GetX2() - xp1;
        Double_t yp1  = gPad->GetY1();
        Double_t dpy  = gPad->GetY2() - yp1;
        
        double x1 = xp1 + dpx * m_X1;
        double y1 = yp1 + dpy * m_Y1;
        
        double x2 = xp1 + dpx * m_X2;
        double y2 = yp1 + dpy * m_Y2;

        SetFillColor( kWhite );
        TPave::PaintPave(x1,y1, x2,y2, GetBorderSize(), opt);
    }

    // Layout constants
    const double c_rowPadding = 0.1;
    const double c_colPadding = 0.03;
    //
    double nRows      = entries.size();    
    double fontSize = (1 - c_rowPadding) / nRows;
    
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
