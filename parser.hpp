
#ifndef RT_ROOT_PARSER__HPP__
#define RT_ROOT_PARSER__HPP__

#include <string>
#include <vector>
#include <iostream>

#include <boost/variant.hpp>
#include <boost/function.hpp>

class Plot;



// Token of the language
typedef boost::variant< int
                      , double
                      , std::string
                      > Token;
#define YYSTYPE Token

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
    
    State state;
    
    // For graph
    std::vector<double> xs,ys;
};


typedef boost::function<void(Plot*,Parser*)> Closure;

// Poor man's closures for delaying action.
class ParserAction {
public:
    virtual ~ParserAction() {}
    virtual void perform(Plot* plot, Parser* parser) = 0;
};

#endif /* RT_ROOT_PARSER__HPP__ */
