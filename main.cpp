
#include "RtPlot.hpp"

#include <cstdlib>
#include <iostream>





int main(int argc, char** argv)
{
    // ROOT initialization
    RtPlot app;

    // Watch for stdin
    // TFileHandler fh( STDIN_FILENO , TFileHandler::kRead );
    // fh.Add();


    // std::cout << "AAA\n";
    app.Run();

    return 0;
}
