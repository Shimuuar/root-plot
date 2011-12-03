#ifndef RT_PLOT_EXCEPTIOS__HPP__
#define RT_PLOT_EXCEPTIOS__HPP__

#include <stdexcept>

// Error during parsing
class ParseError : public std::runtime_error
{
public:
    ParseError(const std::string& str) : std::runtime_error(str) {}
};


#endif /* RT_PLOT_EXCEPTIOS__HPP__ */
