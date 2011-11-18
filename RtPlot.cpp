
#include <iostream>

#include "RtMainFrame.hpp"
#include "reader.hpp"
#include "object.hpp"
#include "parser.hpp"



#include "RtPlot.hpp"
//#include "reader.hpp"


static const char* dummy_argv[] = {"rt-plot"};
static int         dummy_argc   = 1;


RtPlot::RtPlot() :
    TApplication( "rt-plot", &dummy_argc, const_cast<char**>( dummy_argv ) ),
    reader(    new LineReader(STDIN_FILENO) ),
    parser(    new Parser ),
    fdWatcher( new TFileHandler(STDIN_FILENO , TFileHandler::kRead) )
{
    // Set up notification
    fdWatcher->Add();
    TQObject::Connect(fdWatcher, "Notified()",
                      "RtPlot", this, "readMoreData()");
    // Create window
    // FIXME: should work even without X
    // RtMainFrame* bp = new RtMainFrame( gClient->GetRoot() );
    // plot = new Plot( bp->getCanvas() );
}

RtPlot::~RtPlot()
{
    delete fdWatcher;
    delete parser;
    delete reader;
    delete plot;
}

void RtPlot::readMoreData() {
    // No more data from stdin
    if( reader->eof() ) {
        delete fdWatcher;
        fdWatcher = 0;
    }

   std::string str;
    if( reader->getLine( str ) == LineReader::OK ) {
        std::cout << "================\n";
        std::cout << "> '" << str << "'\n";
        parser->feedLine( plot, str );
    }
}

// ROOT
void RtPlot::Streamer(TBuffer&) {}
