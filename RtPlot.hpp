#ifndef RrT_ROOT_RTPLOT__HPP__
#define RT_ROOT_RTPLOT__HPP__

#include "reader.hpp"

#include <TApplication.h>
#include <RQ_OBJECT.h>



// Main application for rt-plot
class RtPlot : public TApplication {
    RQ_OBJECT()
public:
    RtPlot();
    virtual ~RtPlot();

    // Read more data from standard input
    void readMoreData();
private:
    LineReader reader;

public:
    // CINT stuff
    ClassDef(RtPlot,1);
};

#endif /* RT_ROOT_RTPLOT__HPP__ */
