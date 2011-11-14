#ifndef ROOT_READER__HPP__
#define ROOT_READER__HPP__

#include <vector>
#include <string>
#include <boost/noncopyable.hpp>

// Class for reading data from FD. Could be used in conjustion with
// poll/select. Buffering of stdlib getline breaks them.
class LineReader : public boost::noncopyable {
public:
    enum Result {
        OK     = 0,             // Line read succesfully
        NoData,                 // Not enough data
        EOF                     // EOF reached
    };
    
    // Create line reader which reads from fd. FD is set into
    // nonblocking mode.
    LineReader(int fd);
    
    // Read line from FD. If read read is not succesful returns false
    // and content of string doesn't change.
    Result getLine(std::string& str);

private:
    // Try to find line in the buffer. If line fould it's placed into
    // std::string
    bool findLine(std::string& str);
    
    int               fd;       // File descriptor
    std::vector<char> buf;      // Data buffer
    size_t            nBytes;   // Number of bytes in buffer
};


#endif /* ROOT_READER__HPP__ */
