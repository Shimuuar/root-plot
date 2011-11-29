
#ifndef RT_ROOT_PARSER__HPP__
#define RT_ROOT_PARSER__HPP__

#include <string>
#include <vector>
#include <iostream>

#include <boost/variant.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>

class Plot;
class Parser;

// Token of the language
typedef boost::variant<int, double, std::string> Token;
#define YYSTYPE Token

// Closure for the parser. 
typedef boost::function<void(Plot*,Parser*)> Closure;


class LineAccum;

// Line parser
class Parser {
public:
    // Construct parser
    Parser();

    // Feed line to the parser
    void feedLine(Plot* plot, const std::string& str);
private:
    enum State {
        Command,
        Graph
    };

    void procCommand(Plot* plot, const std::string& str );
    void procGraph(  Plot* plot, const std::string& str );

    boost::shared_ptr<LineAccum> accum;
};

#endif /* RT_ROOT_PARSER__HPP__ */
