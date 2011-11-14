
#include "RtPlot.hpp"

#include <TSysEvtHandler.h>

int main(int argc, char** argv)
{
    // ROOT initialization
    RtPlot app;

    // Watch for stdin
    TFileHandler fh( STDIN_FILENO , TFileHandler::kRead );
    TQObject::Connect(&fh, "Notified()", "RtPlot", &app, "readMoreData()");
    
    app.Run();
    return 0;
}
