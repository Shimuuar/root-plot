#ifndef RT_ROOT_GUI__HPP__
#define RT_ROOT_GUI__HPP__

#include <TGFrame.h>
#include <TCanvas.h>
#include <RQ_OBJECT.h>



// Just a GUI holding canvas
class RtMainFrame : public TGMainFrame {
    RQ_OBJECT()
public:
    // Create frame
	RtMainFrame(const TGWindow* p);
    virtual ~RtMainFrame();

    // Get canvas
    TCanvas* getCanvas() { return canvas; }
private:
    TCanvas* canvas;
public:
    // CINT stuff
    ClassDef(RtMainFrame,1);
};

#endif /* RT_ROOT_GUI__HPP__ */
