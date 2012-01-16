
#include <iostream>

#include "RtMainFrame.hpp"
#include "reader.hpp"
#include "object.hpp"
#include "parser.hpp"



#include "RtPlot.hpp"
//#include "reader.hpp"


static const char* dummy_argv[] = {"rt-plot"};
static int         dummy_argc   = 1;


RtPlot::RtPlot(bool verbose, bool batch) :
    TApplication( "rt-plot", &dummy_argc, const_cast<char**>( dummy_argv ) ),
    m_reader(    new LineReader(STDIN_FILENO) ),
    m_parser(    new Parser ),
    m_fdWatcher( new TFileHandler(STDIN_FILENO , TFileHandler::kRead) ),
    m_verbose( verbose ),
    m_batch  ( batch   )
{
    // Set up notification
    m_fdWatcher->Add();
    TQObject::Connect(m_fdWatcher, "Notified()",
                      "RtPlot", this, "readMoreData()");
    // Create canvas to drow upon
    if( getenv("DISPLAY") == 0 ) {
        // No graphics
        TCanvas* cnv = new TCanvas;
        m_plot = new Plot( cnv );
    } else {
        // We have X. Let create window
        RtMainFrame* bp = new RtMainFrame( gClient->GetRoot() );
        m_plot = new Plot( bp->getCanvas() );
    }
}

RtPlot::~RtPlot()
{
    delete m_fdWatcher;
    delete m_parser;
    delete m_reader;
    delete m_plot;
}

void RtPlot::readMoreData() {
    // No more data from stdin
    if( m_reader->eof() ) {
        if( m_batch ) {
            gApplication->Terminate();
        }
        delete m_fdWatcher;
        m_fdWatcher = 0;
    }

    std::string str;
    while( m_reader->getLine( str ) == LineReader::OK ) {
        if( m_verbose )
            std::cout << "> '" << str << "'\n";
        m_parser->feedLine( m_plot, str );
    }
}

// ROOT
void RtPlot::Streamer(TBuffer&) {}
