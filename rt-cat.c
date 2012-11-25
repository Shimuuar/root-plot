#define _XOPEN_SOURCE 500

#include "socket.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <fcntl.h>
#include <unistd.h>


#define BUF_SIZE 4096

// Dump file descriptor to he socket
void dump_fd(int sock, int fd) {
    int n;
    char  buffer[BUF_SIZE];
    while( 1 ) {
        n = read(fd, buffer, BUF_SIZE);
        if( n == 0 )
            break;
        if( n <  0 ) {
            perror("read");
            exit(1);
        }
        if (send(sock, buffer, n, 0) == -1) {
            perror("send");
            exit(1);
        }
    }
}

// Dump file content to the socket
void dump_file(int sock, const char* fname) {
    int fd = open(fname, O_RDONLY);
    if( fd == -1 ) {
        perror("rt-echo: open:");
        exit(1);
    }
    dump_fd(sock, fd);
    close(fd);
}

int main(int argc, char** argv)
{
    int s;
    struct sockaddr_un remote;
    char* sock_path = 0;

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
    if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        exit(1);
    }
    // Connect
    remote.sun_family = AF_UNIX;
    strcpy(remote.sun_path, sock_path); // FIXME: Buffer overrun!
    int len = strlen(remote.sun_path) + sizeof(remote.sun_family);
    if (connect(s, (struct sockaddr *)&remote, len) == -1) {
        perror("connect");
        exit(1);
    }

    // Dump stdin to socket
    if( optind == argc ) {
        dump_fd( s, STDIN_FILENO );
    } else {
        for(int i = optind; i < argc; i++)
            dump_file( s, argv[i] );
    }
    close(s);
    return 0;
}
