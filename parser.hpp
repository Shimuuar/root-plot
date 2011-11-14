
#ifndef ROOT_PARSER__HPP__
#define ROOT_PARSER__HPP__

#include <string>
#include <deque>
#include <iostream>

#include <boost/variant.hpp>


// Keyword in the language
struct Keyword {
    std::string word;

    Keyword(const std::string& str) :
        word(str)
    {}
};
std::ostream& operator << (std::ostream& out, const Keyword& k) {
    return out << "{Keyword} " << k.word;
}

struct WhiteSpace {};
std::ostream& operator << (std::ostream& out, const WhiteSpace& k) {
    return out << "{WhiteSpace}";
}

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
    
};
#endif /* ROOT_PARSER__HPP__ */
