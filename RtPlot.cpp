
#include <iostream>


#include "RtPlot.hpp"
//#include "reader.hpp"


static const char* dummy_argv[] = {"rt-plot"};
static int         dummy_argc   = 1;


RtPlot::RtPlot() :
    TApplication( "rt-plot", &dummy_argc, const_cast<char**>( dummy_argv ) ),
    reader(STDIN_FILENO),
    fh( new TFileHandler(STDIN_FILENO , TFileHandler::kRead) )
{
    // Set up notification
    fh->Add();
    TQObject::Connect(fh, "Notified()", "RtPlot", this, "readMoreData()");
}

RtPlot::~RtPlot()
{
    delete fh;
}

void RtPlot::readMoreData() {
    // No more data from stdin
    if( reader.eof() ) {
        delete fh;
        fh = 0;
    }
   
    std::string str;
    if( reader.getLine( str ) == LineReader::OK ) {
        std::cout << "Got <" << str << "> n=" << str.size() << std::endl;
    }
}

