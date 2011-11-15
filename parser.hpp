
#ifndef RT_ROOT_PARSER__HPP__
#define RT_ROOT_PARSER__HPP__

#include <string>
#include <deque>
#include <vector>
#include <iostream>

#include <boost/variant.hpp>

class Plot;

// Keyword in the language
struct Keyword {
    std::string word;

    Keyword(const std::string& str) :
        word(str)
    {}
};

// Whitespace token
struct WhiteSpace {};


// Token of the language
typedef boost::variant< int
                      , double
                      , std::string
                      , Keyword
                      , WhiteSpace
                      >
        Token;


// After lexing line is just a sequence of tokens
typedef std::deque<Token> LexedLine;


// Lex line into accumulator
bool lexLine(const std::string& str, LexedLine& res);

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


// ================================================================
// HELPERS
inline std::ostream& operator << (std::ostream& out, const Keyword& k) {
    return out << "{Keyword} " << k.word;
}
inline std::ostream& operator << (std::ostream& out, const WhiteSpace&) {
    return out << "{WhiteSpace}";
}

#endif /* RT_ROOT_PARSER__HPP__ */
