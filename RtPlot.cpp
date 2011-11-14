
#include "RtPlot.hpp"

static const char* dummy_argv[] = {"rt-plot"};
static int         dummy_argc   = 1;


RtPlot::RtPlot() :
    TApplication( "rt-plot", &dummy_argc, const_cast<char**>( dummy_argv ) )
{
}

RtPlot::~RtPlot()
{
}

