#ifndef RrT_ROOT_RTPLOT__HPP__
#define RT_ROOT_RTPLOT__HPP__

#include <TApplication.h>
#include <TSysEvtHandler.h>
#include <RQ_OBJECT.h>



class Parser;
class LineReader;
class Plot;

// Main application for rt-plot
class RtPlot : public TApplication {
    RQ_OBJECT()
public:
    RtPlot(bool verbose);
    virtual ~RtPlot();

    // Read more data from standard input
    void readMoreData();
private:
    LineReader*   m_reader;       // Reader for stdin
    Parser*       m_parser;       // Parser
    Plot*         m_plot;         // Plot to draw on
    TFileHandler* m_fdWatcher;    // Notify when there is data to read
    bool          m_verbose;      // Verbose mode
public:
    // CINT stuff
    ClassDef(RtPlot,1);
    // void Streamer(TBuffer&) {};
private:

};


#endif /* RT_ROOT_RTPLOT__HPP__ */
