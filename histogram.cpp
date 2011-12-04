#include "parser.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <list>
#include <boost/lexical_cast.hpp>
#include <boost/make_shared.hpp>

#include <TH1.h>
#include <TH2.h>

// ================================================================
// == Utils
// ================================================================

/** Parse string with given prefix */
template<typename T>
T parseString(const std::string& str, const std::string& prefix)
{
    // Check for header
    parseString<void>(str, prefix);
    // Cast value
    try {
        return boost::lexical_cast<T>( str.substr(prefix.size()) );
    } catch( const boost::bad_lexical_cast& ) {
        throw ParseError("Bad value!");
    }
}
template<>
void parseString<void>(const std::string& str, const std::string& prefix) 
{
    std::string pref = str.substr(0, prefix.size());
    if( pref != prefix ) {
        throw ParseError("Bad format: '" + pref + "' instead of '" + prefix + "'" );
    }
}



// ================================================================
// Implementation of AccumHist
// ================================================================

class AccumHist::Private {
public:
    Private() :
        inHeader(true),
        ok(true)
    {}
    std::string pop();
    void        add(const std::string& s);

    // Parse histogram
    void parseHistogram();
    void parseHistogram1D(const std::string& bin);
    void parseHistogram2D();
    // Parse individual bin types
    void parseBinI  (int& n, double& min, double& max);
    void parseBinInt(int& n, double& min, double& max);
    void parseBinF  (int& n, double& min, double& max);
    void parseBin1D (const std::string& name, int& n, double& min, double& max);
    

    std::list<std::string> header;   // Header parser.
    bool                   inHeader; // Do we parse header.
    bool                   ok;       // Is accumulator in valid state.
    std::auto_ptr<TH1>     hist;     // Point to histogram being built. Initially null.
    int                    nDim;     // Number of dimensions 1 or 2
};

std::string AccumHist::Private::pop() {
    if( header.empty() )
        throw ParseError("Header is too short");
    std::string s = header.front();
    header.pop_front();
    return s;
}

void AccumHist::Private::add(const std::string& s) {
    header.push_back(s);
}

void AccumHist::Private::parseBinI  (int& n, double& min, double& max) {
    int lo,hi;
    lo = parseString<int>(pop(), "# Low  = ");
    hi = parseString<int>(pop(), "# High = ");
    n   = hi - lo + 1;
    min = lo - 0.5;
    max = hi + 0.5;
}
void AccumHist::Private::parseBinInt(int& n, double& min, double& max) {
    int base,size;
    base = parseString<int>(pop(), "# Base = ");
    size = parseString<int>(pop(), "# Step = ");
    n    = parseString<int>(pop(), "# Bins = ");
    min  = base;
    max  = base + n*size;
}
void AccumHist::Private::parseBinF  (int& n, double& min, double& max) {
    double step;
    min  = parseString<double>(pop(), "# Base = ");
    step = parseString<double>(pop(), "# Step = ");
    n    = parseString<int>(pop(),    "# N    = ");
    max = min + step*n;
}

void AccumHist::Private::parseBin1D(const std::string& name, int& n, double& min, double& max) {
    if( name == "# BinPermute" ) {
        pop();
        parseBin1D(pop(), n, min, max);
    } else if( name == "# BinI" ) {
        parseBinI(n, min, max);
    } else if( name == "# BinInt" ) {
        parseBinInt(n, min, max);
    } else if( name == "# BinF" ) {
        parseBinF(n, min, max);
    } else if( name == "# BinD" ) {
        parseBinF(n, min, max);
    } else {
        throw ParseError("unknown bin type '" + name + "'");
    }
}

void AccumHist::Private::parseHistogram1D(const std::string& bin) {
    int    nx;
    double xMin, xMax;
    parseBin1D(bin, nx, xMin, xMax);
    hist = std::auto_ptr<TH1>( new TH1D("FOO", "", nx, xMin, xMax ) );
}

void AccumHist::Private::parseHistogram2D() {
    int nx,ny;
    double xMin, xMax;
    double yMin, yMax;
    parseString<void>(pop(), "# X");
    parseBin1D       (pop(), nx, xMin, xMax);
    parseString<void>(pop(), "# Y");
    parseBin1D       (pop(), ny, yMin, yMax);
    // Read histogram body
    std::auto_ptr<TH2D> h( new TH2D("FOO", "",
                                    nx, xMin, xMax,
                                    ny, yMin, yMax) );
}

void AccumHist::Private::parseHistogram() {
    if( pop() != "# Histogram" )
        throw ParseError("Not a histogram");
    parseString<void>(pop(), "# Underflows = ");
    parseString<void>(pop(), "# Overflows  = ");
    // Get bin
    std::string bin = pop();
    if( bin == "# Bin2D" ) {
        parseHistogram2D();
        nDim = 2;
    } else {
        parseHistogram1D(bin);
        nDim = 1;
    }
};



// ----------------------------------------------------------------

AccumHist::AccumHist() :
    p( new AccumHist::Private )
{}

AccumHist::~AccumHist()
{}

bool AccumHist::feedLine(const std::string& str) {
    if( !p->ok )
        return false;
    try {
        if( p->inHeader && str.size() > 0 && str[0] == '#' ) {
            // We are reading header
            p->add( str );
        } else {
            // We just finished reading header
            if( p->inHeader ) {
                p->parseHistogram();
                p->inHeader = false;
            }
            // Read data
            if( p->nDim == 1 ) {
                double x,y;
                p->ok = 2 == sscanf(str.c_str(),"%lf %lf", &x, &y);
                if( p->ok )
                    p->hist->Fill(x,y);
            } else {
                double x,y,z;
                p->ok = 3 == sscanf(str.c_str(), "(%lf,%lf) %lf", &x, &y, &z);
                // EVIL. Kludgely reinterpret pointer type.
                TH2* h = dynamic_cast<TH2*>( p->hist.get() );
                if( h == 0 ) {
                    std::cerr << "rt-biplot: internal error. Histogram is not 2D\n";
                    return false;
                }
                if( p->ok )
                    h->Fill(x,y,z);
            }
        }
        return p->ok;
    } catch (const ParseError& err ) {
        std::cerr << "rt-plot: " << err.what() << std::endl;
        return p->ok = false;
    }
}

bool AccumHist::flush(Plot* plot) {
    if( p->ok && p->hist.get() ) {
        TH1* h = p->hist.release();
        plot->pushObject( boost::make_shared<PlotHist>( h ) );
        return true;
    }
    return false;
}
