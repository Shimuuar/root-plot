#define _XOPEN_SOURCE 500

#include "rt-plot.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>

#include <pwd.h>
#include <unistd.h>


// Number of connections to keep in queue
#define BACKLOG  25
// Size of buffer
#define BUF_SIZE 4096


int main(int argc, char** argv)
{
    int s, s2;
    struct sockaddr_un remote;
    char* sock_path = 0;
    char  buffer[BUF_SIZE];

    // Parse command line paramters
    for( int c; ((c = getopt (argc, argv, "hs:")) != -1); ) {
        switch( c ) {
        case 's':
            sock_path = optarg;
            break;
        case 'h':
            fprintf(stderr,
                "Usage:\n"
                "    rt-listen [flags]\n"
                "\n"
                "    Listens on the UNIX socket and dumps recieved information to the\n"
                "    standard output. Default path to the socket is /tmp/$USER\n"
                "\n"
                "    -s path - path to the socket\n"
                "    -h      - this message\n"
                );
            exit(1);
        }
    }

    if( -1 == (s = rt_listen( sock_path, BACKLOG ) ) ) {
        printf("NONNNN\n");
        perror("rt-listen");
        exit(1);
    }

    // Main loop
    while( 1 ) {
        socklen_t t = sizeof(remote);
        if( (s2 = accept(s, (struct sockaddr *)&remote, &t)) == -1 ) {
            perror("accept");
            exit(1);
        }
        while( 1 ) {
            // Read data
            int n = recv(s2, buffer, BUF_SIZE, 0);
            if (n == 0)
                break;
            if (n < 0) {
                perror("recv");
                exit(1);
            }
            // Write data to stdout
            write(STDOUT_FILENO, buffer, n);
        }
        // Add line break
        write(STDOUT_FILENO, "\n", 1);
        close(s2);
    }
    return 0;
}
