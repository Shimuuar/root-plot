#define _XOPEN_SOURCE 500
#include "rt-plot.h"

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
    if( 0 != stat(tmpdir, &st) )
        return NULL;
    if( (st.st_mode & S_IFMT) != S_IFDIR )
        return NULL;
    // Get username
    uid = geteuid();
    pw  = getpwuid(uid);
    if( !pw )
        return NULL;
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
    int   len,s;
    char* to_free = NULL;
    struct sockaddr_un remote;

    // Get path to socket
    if( NULL == path )
        to_free = path = rt_default_socket();
    if( NULL == path )
        goto fail;
    // Create socket
    if( (s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
        goto fail;
    // Connect
    remote.sun_family = AF_UNIX;
    strcpy(remote.sun_path, path);
    len = strlen(remote.sun_path) + sizeof(remote.sun_family);
    if (connect(s, (struct sockaddr*)&remote, len) == -1) {
        close( s );
        goto fail;
    }
    free(to_free);
    return s;
fail:
    free(to_free);
    return -1;
}
