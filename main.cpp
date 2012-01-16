
#include "RtPlot.hpp"

#include <iostream>
#include <cstdlib>
#include <signal.h>

#include <TH1.h>
#include <TROOT.h>
#include <TStyle.h>


void usage() {
    std::cerr <<
        "Usage:\n"
        "    rt-plot [flags]\n"
        "\n"
        "Reads commands from standard input and creates a plot\n"
        "\n"
        "    -b - batch mode. terminate after end of input\n"
        "    -x - do not create window\n"
        "    -v - verbose. Echo input to stdout\n"
        "    -h - this message\n"
        ;
    std::exit(1);
}

int main(int argc, char** argv)
{
    // Uninstall SIGSEVG handler
    signal( SIGSEGV, SIG_DFL );
    // Do not histograms to current dir
    TH1::AddDirectory(kFALSE);
    // Reasonable color schemes
    gROOT ->SetStyle("Plain");
    gStyle->SetPalette(1);

    // Parse command line parameters
    bool verbose = false;
    bool batch   = false;
    for( int c; ((c = getopt (argc, argv, "hxvb")) != -1); ) {
        switch(c) {
        case 'x':
            // We don't want graphics so we have to unset display.
            // Otherwise ROOT will create windows
            if( -1 == unsetenv("DISPLAY") ) {
                perror("rt-plot: unable to unset DISPLAY");
                exit(1);
            }
            break;
        case 'b':
            batch = true;
            break;
        case 'v':
            verbose = true;
            break;
        case 'h':
        default :
            usage();
            break;
        }
    }
    RtPlot app( verbose, batch );
    app.Run();
    return 0;
}
