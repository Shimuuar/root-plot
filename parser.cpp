#include "parser.hpp"
#include "parser.l.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <ctype.h>
#include <sstream>
#include <fstream>
#include <boost/make_shared.hpp>

#include <TGraph.h>
#include <TGraphErrors.h>
#include <TPolyLine.h>


namespace {

// ================================================================
// Accumulator which does nothing
class NullAccum : public LineAccum {
public:
    virtual ~NullAccum() {}
    virtual bool flush(Plot*)                  { return true; }
    virtual bool feedLine(const std::string& ) { return true; }
};


// ================================================================
// Accumulator for graphs
class AccumGraph : public LineAccum {
public:
    AccumGraph();
    virtual ~AccumGraph();
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);
protected:
    typedef std::vector<double> Column;
    typedef std::vector<Column> ColList;

    // Data columns
    ColList cols;
    Column  surrogateX;
    size_t  colSize()     { return cols[0].size();  }

    // Indices for columns
    int i_x;      // X
    int i_dx;     // Symmetric error for X
    int i_udx;    // Upper error for X
    int i_ldx;    // Lower error for X
    int i_y;      // Y
    int i_dy;     // Symmetric error for Y
    int i_udy;    // Upper error for Y
    int i_ldy;    // Lower error for Y

    bool noHeader();   // Ho header is given
    bool unusableData(); // Some indices are out of range
    // Getter for columns
    double* getX();
    double* getDX()  { return getCol( i_dx );  }
    double* getUDX() { return getCol( i_udx ); }
    double* getLDX() { return getCol( i_ldx ); }
    double* getY()   { return getCol( i_y );   }
    double* getDY()  { return getCol( i_dy );  }
    double* getUDY() { return getCol( i_udy ); }
    double* getLDY() { return getCol( i_ldy ); }
    double* getCol(int i);
};

AccumGraph::AccumGraph() :
    i_x(-1), i_dx(-1), i_udx(-1), i_ldx(-1),
    i_y(-1), i_dy(-1), i_udy(-1), i_ldy(-1)
{}

AccumGraph::~AccumGraph()
{}

bool AccumGraph::noHeader() {
    return i_x < 0 && i_dx < 0 && i_udx < 0 && i_ldx < 0 &&
           i_y < 0 && i_dy < 0 && i_udy < 0 && i_ldy < 0;
}

bool AccumGraph::unusableData() {
    int n = cols.size();
    return i_x >= n || i_dx >= n || i_udx >= n || i_ldx >= n ||
           i_y >= n || i_dy >= n || i_udy >= n || i_ldy >= n ||
           n == 0;
}

double* AccumGraph::getX() {
    double* x = getCol( i_x );
    if( x == 0 ) {
        size_t n = colSize();
        surrogateX.resize( n );
        for(unsigned i = 0; i < n; i++)
            surrogateX[i] = i;
        x = &surrogateX[0];
    }
    return x;
}

double* AccumGraph::getCol(int i) {
    int n = cols.size();
    if( i < 0 || i >= n )
        return 0;
    else
        return &(cols[i][0]);
}

bool AccumGraph::feedLine(const std::string& str) {
    // Lex header if heeded
    if( cols.size() == 0 && noHeader() && str.size() > 0 && str[0] == '#' ) {
        // FIXME: parse it!
    }
    // Lexing buffer line
    std::vector<int> offs;
    std::string s = str;    
    // Skip initial whitespaces;
    unsigned i = 0;
    while( i < s.length() && isspace(s[i]) )
        i++;
    // Start lexing
    bool wordEnded = true;
    for(; i < s.length(); i++) {
        if( isspace( s[i] ) ) {
            wordEnded = true;
            s[i] = '0';
        } else if (wordEnded) {
            offs.push_back( i );
            wordEnded = false;
        }
    }
    // Resize columns array if needed
    if( cols.size() == 0 ) {
        cols.resize( offs.size() );
    }
    if( cols.size() != offs.size() ) {
        return false;
    }
    // Read every number
    for( unsigned j = 0; j < offs.size(); j++) {
        double x;
        if( 1 != sscanf( s.c_str() + offs[j], "%lf", &x ) )
            return false;
        cols[j].push_back( x );
    }
    return true;
}

bool AccumGraph::flush(Plot* plot) {
    // Use default column number if none have been given
    if( noHeader() ) {
        switch( cols.size() ) {
        case 1:
            i_y  = 0;
            break;
        case 2:
            i_x  = 0;
            i_y  = 1;
            break;
        case 3:
            i_x  = 0;
            i_y  = 1;
            i_dy = 2;
            break;
        }
    }
    // Check that indices are OK
    if( unusableData() )
        return false;
    // Get columns
    double *x  = getX();
    double *y  = getY();
    double *dx = getDX();
    double *dy = getDY();
    // Check mandatory columns
    if( !x || !y )
        return false;
    // We have error bars
    if( dx || dy ) {
        plot->pushObject(
            boost::make_shared<PlotGraph>(
                new TGraphErrors( colSize(), x, y, dx, dy ) ) );
        return true;
    }
    // Plain old graphs
    plot->pushObject(
        boost::make_shared<PlotGraph>(
            new TGraph( colSize(), x, y ) ) );
    return true;
}


// ================================================================
// Accumulator for barchart
class AccumBarchart : public AccumGraph {
public:
    virtual ~AccumBarchart();
    virtual bool flush(Plot*);
};

AccumBarchart::~AccumBarchart()
{}

bool AccumBarchart::flush(Plot* plot) {
    // Use default column number if none have been given
    if( noHeader() ) {
        switch( cols.size() ) {
        case 2:
            i_x  = 0;
            i_y  = 1;
            break;
        }
    }
    // Check that indices are OK
    if( unusableData() )
        return false;
    // Get columns
    double *x  = getX();
    double *y  = getY();
    if( x && y ) {
        plot->pushObject(
            boost::make_shared<PlotBarChart>(
                new TGraph( colSize(), x, y ) ) );
        return true;
    } else {
        return false;
    }
}

// ================================================================
// Accumulator for polygons
class AccumPoly : public AccumGraph {
public:
    virtual ~AccumPoly();
    virtual bool flush(Plot*);
};

AccumPoly::~AccumPoly()
{}

bool AccumPoly::flush(Plot* plot) {
    int n = cols.size();
    if( i_x < 0 || i_x >= n ||
        i_y < 0 || i_y >= n || colSize() < 3)
    {
        return false;
    }
    std::vector<double>& xs = cols[0];
    std::vector<double>& ys = cols[1];
    xs.push_back( xs[0] );
    ys.push_back( ys[0] );
    plot->pushObject(
        boost::make_shared<PlotPoly>(
            new TPolyLine( xs.size(), &(xs[0]), &(ys[0])) ) );
    return true;
}

} // namespace


// ================================================================ //
PLineAccum makeNullAccum() {
    return boost::make_shared<NullAccum>();
}

PLineAccum makeAccumGraph() {
    return boost::make_shared<AccumGraph>();
}

PLineAccum makeAccumPoly() {
    return boost::make_shared<AccumGraph>();
}

PLineAccum makeAccumBarchart() {
    return boost::make_shared<AccumBarchart>();
}

bool LineAccum::readFromFile(const std::string& fname, Plot* plot) {
    // Open file
    std::ifstream in( fname.c_str() );
    if( !in.good() )
        return false;
    // Read line-by line
    std::string str;
    while( !std::getline(in, str).eof() ) {
        if( ! feedLine( str ) )
            return false;
    }
    return flush( plot );
}

Parser::Parser()
{
}

void Parser::feedLine(Plot* plot, const std::string& str) {
    if( accum ) {
        // Did we hit end of data marker?
        bool endOfData = str.size() >= 3
                      && str[0] == '<' && str[1] == '<' && str[2] == '<';
        for(size_t i = 3; endOfData && i < str.size(); i++ )
            endOfData = endOfData && isspace( str[i] );

        if( endOfData ) {
            // Flush and delete accumulator
            if( ! accum->flush( plot ) ) {
                std::cerr << "rt-plot: cannot add inline data to the plot";
            }
            accum.reset();
        } else {
            // Push one line to accumulator
            if( !accum->feedLine(str) ) {
                std::cerr << "rt-plot: cannot parse inline data dropping to null parser\n";
                accum = makeNullAccum();
            }
        }
    } else {
        // We are parsing DSL
        YY_BUFFER_STATE state;
        state = yy_scan_string( str.c_str() );
        if( 0 != yyparse( ParseParam(this, plot) ) ) {
            std::cerr << "  in string: '" << str << "'\n";
        }
        // Parse completed
        yy_delete_buffer( state );
    }

    // Redraw everything
    if( !accum )
        plot->draw();
}
