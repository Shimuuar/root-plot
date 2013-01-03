#ifndef RT_ROOT_GUI__HPP__
#define RT_ROOT_GUI__HPP__

#include <TGFrame.h>
#include <TCanvas.h>
#include <TGToolTip.h>
#include <RQ_OBJECT.h>

class Plot;


// Just a GUI holding canvas
class RtMainFrame : public TGMainFrame {
    RQ_OBJECT()
public:
    // Create frame
	RtMainFrame(const TGWindow* p);
    virtual ~RtMainFrame();

    // Get canvas
    TCanvas* getCanvas() { return m_canvas; }
    // Set plot
    void setPlot(Plot* p) { m_plot = p; }

private:
    TCanvas*   m_canvas;
    TGToolTip* m_tip;
    Plot*      m_plot;   // Not owned;

public:
    // Handler for saving PDF
    void savePDF();
    // Event handler
    void eventHandler(Int_t event, Int_t px, Int_t py, TObject *selected);

    // CINT stuff
    ClassDef(RtMainFrame,1);
};


#endif /* RT_ROOT_GUI__HPP__ */
