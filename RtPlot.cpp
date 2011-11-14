
#include <iostream>
#include "RtPlot.hpp"
//#include "reader.hpp"


static const char* dummy_argv[] = {"rt-plot"};
static int         dummy_argc   = 1;


RtPlot::RtPlot() :
    TApplication( "rt-plot", &dummy_argc, const_cast<char**>( dummy_argv ) ),
    reader(STDIN_FILENO)
{
}

RtPlot::~RtPlot()
{
}

void RtPlot::readMoreData() {
    if( reader.eof() )
        return;
    std::cout << "<<Reading>>\n";
    
    std::string str;
    if( reader.getLine( str ) == LineReader::OK ) {
        std::cout << "Got <" << str << "> n=" << str.size() << std::endl;
    }
}

