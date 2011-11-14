#ifndef ROOT_RTPLOT__HPP__
#define ROOT_RTPLOT__HPP__

#include <TApplication.h>
#include <RQ_OBJECT.h>

// Main application for rt-plot
class RtPlot : public TApplication {
    RQ_OBJECT()
public:
    RtPlot();
    virtual ~RtPlot();
private:
};

#endif /* ROOT_RTPLOT__HPP__ */
