
#ifndef RT_ROOT_PARSER__HPP__
#define RT_ROOT_PARSER__HPP__

#include <string>
#include <iostream>

#include <boost/variant.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>



class Plot;
class Parser;
class LineAccum;

// Token of the language
typedef boost::variant<int, double, std::string> Token;
// Define token type for bison
#define YYSTYPE Token

// Closure for the parser. To be performed actions are stored as
// closures.
typedef boost::function<void(Plot*,Parser*)> Closure;


// Line parser
class Parser {
public:
    // Construct parser
    Parser();

    // Feed line to the parser
    void feedLine(Plot* plot, const std::string& str);
private:
    // Pointer to current accumulator
    boost::shared_ptr<LineAccum> accum;
};

#endif /* RT_ROOT_PARSER__HPP__ */
