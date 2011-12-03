
#include "reader.hpp"

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>



LineReader::LineReader(int fd_) :
    fd(fd_),
    buf(4096),
    nBytes(0),
    done(false)
{
    fcntl(fd, F_SETFL, O_NONBLOCK);
}

bool LineReader::findLine(std::string& str) {
    char* start = &buf[0];
    char* last  = &buf[0] + nBytes;
    char* end   = start;
    while( *end != '\n' && end++ <= last );
    if( end <= last ) {
        size_t size = end - start + 1;
        str         = std::string(start, size-1);
        // Move data to 0 offset
        for(size_t i = 0; i < nBytes; i++)
            buf[i] = buf[size+i];
        nBytes -= size;
        return true;
    }
    return false;
}

LineReader::Result LineReader::getLine(std::string& str) {
    // Try to find line in the buffer
    if( findLine( str ) )
        return OK;

    // Unsuccesful. Try to read more
    if( nBytes == buf.size() ) {
        throw "FIXME: Too long line";
    }
    ssize_t n = read(fd, &buf[0]+nBytes, buf.size() - nBytes);

    // End of file.  
    if( n == 0  ) {
        done = true;
        // If buffer still contain data return it. It must be single line
        if( nBytes > 0 ) {
            str    = std::string( &buf[0], nBytes );
            nBytes = 0;
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
    nBytes += n;
    if( findLine( str ) )
        return OK;
    else
        return NoData;
}
