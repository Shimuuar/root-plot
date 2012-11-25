#ifndef RT_ROOT_SOCKET__H__
#define RT_ROOT_SOCKET__H__

// Default path to the socket
char* rt_default_socket();

// Connect to the socket
int rt_connect(char* path);

#endif /* RT_ROOT_SOCKET__H__ */
