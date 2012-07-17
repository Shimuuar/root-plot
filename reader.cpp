#include <iostream>
#include "reader.hpp"

#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>



LineReader::LineReader(int fd_) :
    fd(fd_),
    buf(4096),
    first(0),
    last(0),
    done(false)
{
    fcntl(fd, F_SETFL, O_NONBLOCK);
}

bool LineReader::findLine(std::string& str) {
    if( first >= last )
        return false;
    // Find EOL
    size_t i = first;
    while( i < last && buf[i] != '\n' )
        i++;
    // EOL is found
    if( i < last && buf[i] == '\n' ) {
        str   = std::string( &buf[first], i - first );
        first = i+1;
        return true;
    }
    return false;
}

LineReader::Result LineReader::getLine(std::string& str) {
    // Try to find line in the buffer
    if( findLine( str ) )
        return OK;

    // Move data to beginning of buffer
    memmove( &buf[0], &buf[first], last - first );
    last  -= first;
    first  = 0;
    // Unsuccesful. Try to read more
    if( first == 0 && last == buf.size() ) {
        throw "FIXME: Too long line";
    }
    ssize_t n = read(fd, &buf[last], buf.size() - last);

    // End of file.  
    if( n == 0  ) {
        done = true;
        // If buffer still contain data return it. It must be single line
        if( first < last ) {
            str   = std::string( &buf[first], last - first );
            first = last;
            return OK;
        } else {
            return Eof;
        }
    }
    if( n == -1 ) {
        // Early return and no data yet
        if( errno == EAGAIN      ||
            errno == EWOULDBLOCK ||
            errno == EINTR )
            return NoData;
        // Genuine error
        throw "FIXME";
    }

    // Try to find line again
    last += n;
    if( findLine( str ) )
        return OK;
    else
        return NoData;
}
