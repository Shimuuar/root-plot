#include "RtMainFrame.hpp"

#include <string>

#include <TRootEmbeddedCanvas.h>
#include <TGButton.h>

#include "object.hpp"



RtMainFrame::RtMainFrame(const TGWindow* p) :
    TGMainFrame(p, 800, 600)
{
    // Create canvas
    TRootEmbeddedCanvas* eCanvas = new TRootEmbeddedCanvas("Embedded canvas",this,800,600);
    m_canvas = eCanvas->GetCanvas();
	AddFrame(eCanvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,
                                        10, 10, 10, 1));

    // ========================================
    // Tooltip
    m_canvas->Connect("ProcessedEvent(Int_t,Int_t,Int_t,TObject*)",
                       "RtMainFrame", this, "eventHandler(Int_t,Int_t,Int_t,TObject*)");
    m_tip = new TGToolTip( gClient->GetDefaultRoot(), eCanvas, "", 0 );

    // ========================================
    // Buttons
	TGHorizontalFrame *hframe= new TGHorizontalFrame(this, 200,40);
    // Exit button
    hframe->AddFrame( new TGTextButton(hframe, "&Quit", "gApplication->Terminate()")
					, new TGLayoutHints(kLHintsCenterX,5,5,3,4));
    // Save button
    TGTextButton* buttonPDF = new TGTextButton(hframe, "Save &PDF");
    TQObject::Connect(buttonPDF, "Clicked()",
                     "RtMainFrame", this, "savePDF()" );
    hframe->AddFrame( buttonPDF
                    , new TGLayoutHints(kLHintsCenterX,5,5,3,4));
    AddFrame(hframe,new TGLayoutHints(kLHintsCenterX,2,2,2,2));
    
	// Sets window name and shows the main frame
	SetWindowName("RT-Biplot");
	MapSubwindows();
	Resize(GetDefaultSize());
	MapWindow();
}

RtMainFrame::~RtMainFrame() {
}

void RtMainFrame::savePDF() {
    m_canvas->SaveAs("ROOT-Plot.pdf");
}

void RtMainFrame::eventHandler(Int_t event, Int_t px, Int_t py, TObject*)
{
    switch( event ) {
    case kMouseLeave:  {
        m_tip->Hide();
        break;
    }
    case kMouseMotion: {
        m_tip->SetText( m_plot->getTooltip(px, py).c_str() );
        m_tip->SetPosition(px+8, py+8);
        m_tip->Reset();
        break;
    }
    default: ;
    }

}

// ROOT
void RtMainFrame::Streamer(TBuffer&) {}
