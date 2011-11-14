
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
    std::cout << "<<Reading>>\n";
    
    std::string str;
    bool ok = reader.getLine( str );
    if( ok ) {
        std::cout << "Got <" << str << std::endl;
    }
}

