
#include "parser.hpp"

#include <ctype.h>
#include <boost/lexical_cast.hpp>

typedef std::string::const_iterator iter;


// // VERY-VERY-VERY

// // Skip all spaces
// static void skipWS(iter& i, const iter& end) {
//     while( i != end  &&  isspace( *i ) )
//         ++i;
// }

// // Require WS separator
// static void requireWS(iter& i, const iter& end) {
//     if( i == end )
//         return;
//     if( isspace( *i ) )
//         throw "FIXME";
//     skipWS( i , end );
// }



// bool lexLine(const std::string& str, LexedLine& res) {
//     //
//     res.clear();
//     // Start parsing
//     iter i   = str.begin();
//     iter end = str.end();
//     skipWS(i, end);
//     // Find each token
//     try {
//         while( i != end ) {
//             // Keyword
//             if( *i == '-' ) {
//                 res.push_back( Keyword("-") );
//                 requireWS(i, end);
//             }
//             else if( isalpha( *i ) ) {
//                 iter start = i;
//                 ++i;
//                 while( i != end  &&  (isalpha( *i ) || isdigit( *i )) )
//                     ++i;
//                 res.push_back( Keyword( std::string( start, i ) ) );
//                 requireWS(i, end);
//             } 
//             // String literal
//             else if( *i == '"' ) {
//                 ++i;
//                 iter start = i;
//                 while( i != end &&  *i != '"' )
//                     i++;
//                 if( *i != '"' )
//                     throw "FIXME";
//                 res.push_back( std::string( start, i ) );
//                 requireWS(i, end);
//             }
//             // Integer literal

//         }
//     } catch( const char* ) {
//         res.clear();
//         return false;
//     }
//     return true;
// }
