
%{
#include "parser.hpp"
#include <boost/lexical_cast.hpp>
    
#define YY_DECL int lexLineWorker(LexedLine& res)
%}
%option nounput

ident    [a-zA-Z][a-zA-Z0-9]*
int      [-+]?[0-9]+
ws       [ \t]+
strlit   \"[^\"]*\"

%%%

{ident}     res.push_back( Keyword( yytext ) );
{int}       res.push_back( boost::lexical_cast<int>( yytext ) );
{ws}        res.push_back( WhiteSpace() );
{strlit}    {
                std::string s = yytext+1;
                res.push_back( s.substr(0, s.size() - 1) );
            }

%%%

bool lexLine(const std::string& str, LexedLine& res) {
    try {
        res.clear();
        // Set up flex
        yy_scan_string( str.c_str() );
        lexLineWorker( res );
        yy_delete_buffer( YY_CURRENT_BUFFER );
    } catch( ... ) {
        res.clear();
        return false;
    }
    return true;
}
