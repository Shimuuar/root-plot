
#include "RtMainFrame.hpp"

#include <TRootEmbeddedCanvas.h>
#include <TGButton.h>



RtMainFrame::RtMainFrame(const TGWindow* p) :
    TGMainFrame(p, 800, 600)
{
    // Create canvas
    TRootEmbeddedCanvas* eCanvas = new TRootEmbeddedCanvas("Embedded canvas",this,800,600);
    canvas = eCanvas->GetCanvas();
	AddFrame(eCanvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,
                                        10, 10, 10, 1));

    // ========================================
    // Buttons
	TGHorizontalFrame *hframe= new TGHorizontalFrame(this, 200,40);
    // Exit button
    hframe->AddFrame( new TGTextButton(hframe, "&Quit", "gApplication->Terminate()")
					, new TGLayoutHints(kLHintsCenterX,5,5,3,4));
    // Save button
    TGTextButton* buttonPDF = new TGTextButton(hframe, "Save &PDF");
    // TQObject::Connect(buttonPDF, "Clicked()",
    //                  "BiplotMainFrame", this, "savePDF_button()" );
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

// ROOT
void RtMainFrame::Streamer(TBuffer&) {}
