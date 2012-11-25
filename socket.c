#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>

#include <pwd.h>
#include <unistd.h>



// Find default path to the socket. Value is allocated on the heap
char* rt_default_socket() {
    const  char   *tmpdir;
    char          *dir;
    struct stat    st;
    struct passwd *pw;
    uid_t          uid;
    
    // Get temporary directory
    tmpdir = getenv("TMPDIR");
    if( !tmpdir )
        tmpdir = "/tmp";
    // Check that it is a directory
    if( 0 != stat(tmpdir, &st) ) {
        perror("rt-listen");
        exit(1);
    }
    if( (st.st_mode & S_IFMT) != S_IFDIR ) {
        fprintf(stderr, "rt-listen: TMPDIR is not a directory!\n");
        exit(1);
    }
    // Get username
    uid = geteuid();
    pw  = getpwuid(uid);
    if( !pw ) {
        fprintf(stderr, "rt-listen: cannot find username\n");
        exit(1);
    }
    // Fill default path
    dir = (char*)malloc(strlen(tmpdir) + 1 + strlen(pw->pw_name) + 10 + 1);
    strcpy(dir, tmpdir);
    strcat(dir, "/");
    strcat(dir, pw->pw_name);
    mkdir(dir, 0700);           // Create directory
    strcat(dir, "/rt-socket");
    return dir;
}


int rt_connect(char* path) {
    int len,s;
    struct sockaddr_un remote;

    // Get path to socket
    if( path == NULL )
        path = rt_default_socket();
    // Create socket
    if( (s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        return -1;
    }
    // Connect
    remote.sun_family = AF_UNIX;
    strcpy(remote.sun_path, path);
    len = strlen(remote.sun_path) + sizeof(remote.sun_family);
    if (connect(s, (struct sockaddr*)&remote, len) == -1) {
        close( s );
        return -1;
    }
    return s;
}
