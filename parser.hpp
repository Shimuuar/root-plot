
#ifndef RT_ROOT_PARSER__HPP__
#define RT_ROOT_PARSER__HPP__

#include <string>
#include <vector>
#include <iostream>

#include <boost/variant.hpp>
#include <boost/shared_ptr.hpp>

class Plot;

// Keyword in the language
struct Keyword {
    std::string word;

    Keyword(const std::string& str) :
        word(str)
    {}
};

// Token of the language
typedef boost::variant< int
                      , double
                      , std::string
                      , Keyword
                      >
        Token;
#define YYSTYPE Token


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


// ================================================================
// HELPERS
// Equality tests 
inline bool operator == (const Keyword& a,  const Keyword&  b) { return a == b; }
inline bool operator != (const Keyword& a,  const Keyword&  b) { return a != b; }

inline std::ostream& operator << (std::ostream& out, const Keyword& k) {
    return out << "{Keyword} " << k.word;
}

#endif /* RT_ROOT_PARSER__HPP__ */
