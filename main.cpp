
#include "RtPlot.hpp"

#include <cstdlib>
#include <iostream>

#include <TSysEvtHandler.h>

#include <iostream>
#include "reader.hpp"

int main(int argc, char** argv)
{
    // ROOT initialization
    RtPlot app;

    // Watch for stdin
    TFileHandler fh( STDIN_FILENO , TFileHandler::kRead );
    fh.Add();
    TQObject::Connect(&fh, "Notified()", "RtPlot", &app, "readMoreData()");

    // std::cout << "AAA\n";
    app.Run();

    LineReader r(STDIN_FILENO);
    for(int i = 0; i < 10; i++) {
        std::string s;
        if( r.getLine( s ) == LineReader::OK )
            std::cout << s << std::endl;
    }
    return 0;
}
