#define _XOPEN_SOURCE 500

#include "socket.h"

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
    struct sockaddr_un local, remote;
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
    if( !sock_path )
        sock_path = rt_default_socket();

    
    // Create socket
    if( (s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1 ) {
        perror("socket");
        exit(1);
    }
    // Bind socket
    local.sun_family = AF_UNIX;
    strcpy(local.sun_path, sock_path); // FIXME: buffer overrun!!!
    unlink(local.sun_path);
    
    int len = strlen(local.sun_path) + sizeof(local.sun_family);
    if (bind( s, (struct sockaddr *)&local, len) == -1) {
        perror("bind");
        exit(1);
    }

    if (listen(s, BACKLOG) == -1) {
        perror("listen");
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
            int nw = write(STDOUT_FILENO, buffer, n);
            /* fprintf( stderr, "Bytes written %i of %i\n", nw, n); */
        }
        // Add line break
        write(STDOUT_FILENO, "\n", 1);
        close(s2);
    }
    return 0;
}
