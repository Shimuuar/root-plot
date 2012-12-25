#ifndef RT_ROOT_SOCKET__H__
#define RT_ROOT_SOCKET__H__

// Default path to the UNIX socket used by rt-listen
//
// Return value is allocated on he heap and must be freed by caller
// In case of failure NULL returned.
char* rt_default_socket();

// Connect to the socket. If path is NULL then default path is used.
// In case of failure -1 is returned
int rt_connect(char* path);

// Set socket to listen at. If path is NULL then default path is used.
// Returns socket or -1 in case of failure.
int rt_listen(char* path, int maxConn);

#endif /* RT_ROOT_SOCKET__H__ */
