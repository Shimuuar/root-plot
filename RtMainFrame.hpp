#ifndef RT_ROOT_GUI__HPP__
#define RT_ROOT_GUI__HPP__

#include <TGFrame.h>
#include <TRootEmbeddedCanvas.h>
#include <TGButton.h>
#include <RQ_OBJECT.h>

// Just a GUI holding canvas
class RtMainFrame : public TGMainFrame {
    RQ_OBJECT()
public:
    // Create frame
	RtMainFrame(const TGWindow* p);
    virtual ~RtMainFrame();

private:
public:
    // CINT stuff
    ClassDef(RtMainFrame,1);
};

#endif /* RT_ROOT_GUI__HPP__ */
