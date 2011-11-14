
#include <TApplication.h>
#include <TSysEvtHandler.h>

int main(int argc, char** argv)
{
    // ROOT initialization
    int   dummy_argc = 1;
    char* dummy_argv[] = {"rt-biplot"};
    TApplication app("View biplot", &dummy_argc, dummy_argv);

    // Watch for stdin
    TFileHandler fh( STDIN_FILENO , TFileHandler::kRead );

    
    app.Run();
    return 0;
}
