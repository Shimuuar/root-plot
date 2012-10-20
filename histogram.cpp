#include "parser.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <list>
#include <boost/lexical_cast.hpp>
#include <boost/make_shared.hpp>

#include <TH1.h>
#include <TH2.h>


namespace {

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
// Histogram accumulation
// ================================================================

// Accumulator for histograms.
class AccumHist : public LineAccum {
public:
    AccumHist() :
        inHeader(true),
        ok(true)
    {}
    virtual ~AccumHist();
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);

private:
    std::string pop();                     // Get string from header
    void        add(const std::string& s); // Add string to header parser

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

std::string AccumHist::pop() {
    if( header.empty() )
        throw ParseError("Header is too short");
    std::string s = header.front();
    header.pop_front();
    return s;
}

void AccumHist::add(const std::string& s) {
    header.push_back(s);
}

void AccumHist::parseBinI  (int& n, double& min, double& max) {
    int lo,hi;
    lo = parseString<int>(pop(), "# Low  = ");
    hi = parseString<int>(pop(), "# High = ");
    n   = hi - lo + 1;
    min = lo - 0.5;
    max = hi + 0.5;
}
void AccumHist::parseBinInt(int& n, double& min, double& max) {
    int base,size;
    base = parseString<int>(pop(), "# Base = ");
    size = parseString<int>(pop(), "# Step = ");
    n    = parseString<int>(pop(), "# Bins = ");
    min  = base;
    max  = base + n*size;
}
void AccumHist::parseBinF  (int& n, double& min, double& max) {
    double step;
    min  = parseString<double>(pop(), "# Base = ");
    step = parseString<double>(pop(), "# Step = ");
    n    = parseString<int>(pop(),    "# N    = ");
    max = min + step*n;
}

void AccumHist::parseBin1D(const std::string& name, int& n, double& min, double& max) {
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

void AccumHist::parseHistogram1D(const std::string& bin) {
    int    nx;
    double xMin, xMax;
    parseBin1D(bin, nx, xMin, xMax);
    hist = std::auto_ptr<TH1>( new TH1D("FOO", "", nx, xMin, xMax ) );
}

void AccumHist::parseHistogram2D() {
    int nx,ny;
    double xMin, xMax;
    double yMin, yMax;
    parseString<void>(pop(), "# X");
    parseBin1D       (pop(), nx, xMin, xMax);
    parseString<void>(pop(), "# Y");
    parseBin1D       (pop(), ny, yMin, yMax);
    // Read histogram body
    hist = std::auto_ptr<TH1>( new TH2D("FOO", "",
                                        nx, xMin, xMax,
                                        ny, yMin, yMax) );
}

void AccumHist::parseHistogram() {
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


AccumHist::~AccumHist()
{}

bool AccumHist::feedLine(const std::string& str) {
    if( !ok )
        return false;
    try {
        if( inHeader && str.size() > 0 && str[0] == '#' ) {
            // We are reading header
            add( str );
        } else {
            // We just finished reading header
            if( inHeader ) {
                parseHistogram();
                inHeader = false;
            }
            // Read data
            if( nDim == 1 ) {
                double x,y;
                ok = 2 == sscanf(str.c_str(),"%lf %lf", &x, &y);
                if( ok )
                    hist->Fill(x,y);
            } else {
                double x,y,z;
                ok = 3 == sscanf(str.c_str(), "(%lf,%lf) %lf", &x, &y, &z);
                // EVIL. Kludgely reinterpret pointer type.
                TH2* h = dynamic_cast<TH2*>( hist.get() );
                if( h == 0 ) {
                    std::cerr << "rt-biplot: internal error. Histogram is not 2D\n";
                    return false;
                }
                if( ok )
                    h->Fill(x,y,z);
            }
        }
        return ok;
    } catch (const ParseError& err ) {
        std::cerr << "rt-plot: " << err.what() << std::endl;
        return ok = false;
    }
}

bool AccumHist::flush(Plot* plot) {
    if( ok && hist.get() ) {
        TH1* h = hist.release();
        plot->pushObject( boost::make_shared<PlotHist>( h ) );
        return true;
    }
    return false;
}
}//namespace

// ================================================================
PLineAccum makeAccumHist() {
    return boost::shared_ptr<AccumHist>( new AccumHist() );
}
