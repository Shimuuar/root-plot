
#include "RtPlot.hpp"

#include <signal.h>

#include <TH1.h>
#include <TROOT.h>
#include <TStyle.h>



int main()
// int main(int argc, char** argv)
{
    // Uninstall SIGSEVG handler
    signal( SIGSEGV, SIG_DFL );
    // Do not histograms to current dir
    TH1::AddDirectory(kFALSE);
    // Reasonable color schemes
    gROOT ->SetStyle("Plain");
    gStyle->SetPalette(1);

    RtPlot app;
    app.Run();
    return 0;
}
