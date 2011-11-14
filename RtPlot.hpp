#ifndef RrT_ROOT_RTPLOT__HPP__
#define RT_ROOT_RTPLOT__HPP__

#include "reader.hpp"

#include <TApplication.h>
#include <TSysEvtHandler.h>
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
    LineReader    reader;       // Reader for stdin
    TFileHandler* fh;           // Notify when there is data to read
public:
    // CINT stuff
    ClassDef(RtPlot,1);
};

#endif /* RT_ROOT_RTPLOT__HPP__ */
