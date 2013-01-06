#include "parser.hpp"
#include "object.hpp"
#include "exceptions.hpp"

#include <list>
#include <boost/lexical_cast.hpp>
#include <boost/make_shared.hpp>

#include <TH1.h>
#include <TH2.h>

#include "memory.hpp"



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


// Information about bins
class BinInfo {
public:
    struct BinLog {};

    // Normal linear bins
    BinInfo(int n_, double min_, double max_);
    // Logarithmic bins
    BinInfo(int n, double min_, double max_, BinLog);

    // Whether custom bins exists
    bool haveBins() const { return bins.size() > 1; }

    // Get number of bins
    int     getN()    const;
    double  getMin()  const { return min; }
    double  getMax()  const { return max; }
    double* getBins()       { return &bins[0]; }
    // Should bin content be normalized. True if bins are not equal
    bool    shouldNormalize() const { return haveBins(); }
private:
    int    n;
    double min;
    double max;
    std::vector<double> bins;
};

typedef boost::shared_ptr<BinInfo> PBinInfo;

BinInfo::BinInfo(int n_, double min_, double max_) :
    n(n_), min(min_), max(max_)
{
    if( n < 0 || max < min )
        throw ParseError("Bad linear bins");
}

BinInfo::BinInfo(int nLog, double xMin, double xMax, BinLog) :
    bins(nLog + 1)
{
    if( nLog < 1 || xMin <= 0 || xMax <= 0 || xMax < xMin )
        throw ParseError("Bad logarithmic bins");
    //
    double d = log(xMax / xMin) / nLog;
    for( int i = 0; i <= nLog; i++ ) {
        bins[i] = xMin * exp( d * i );
    }
}

int BinInfo::getN() const {
    if( haveBins() )
        return bins.size() - 1;
    else
        return n;
}

TH1* allocHist1D( PBinInfo bin ) {
    if( bin->haveBins() ) {
        return newROOT<TH1D>("FOO","", bin->getN(), bin->getBins());
    } else {
        return newROOT<TH1D>("FOO","", bin->getN(), bin->getMin(), bin->getMax());
    }
}

TH2* allocHist2D( PBinInfo bX, PBinInfo bY) {
    //
    if( bX->haveBins() && bY->haveBins() )
        return newROOT<TH2D>("FOO","",
                        bX->getN(), bX->getBins(),
                        bY->getN(), bY->getBins());
    //
    if( bX->haveBins() )
        return newROOT<TH2D>("FOO","",
                        bX->getN(), bX->getBins(),
                        bY->getN(), bY->getMin(), bY->getMax() );
    //
    if( bY->haveBins() )
        return newROOT<TH2D>("FOO","",
                        bX->getN(), bX->getMin(), bX->getMax(),
                        bY->getN(), bY->getBins());
    //
    return newROOT<TH2D>("FOO","",
                    bX->getN(), bX->getMin(), bX->getMax(),
                    bY->getN(), bY->getMin(), bY->getMax() );
}


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
    PBinInfo parseBinI   ();
    PBinInfo parseBinInt ();
    PBinInfo parseBinF   ();
    PBinInfo parseBinLogD();
    PBinInfo parseBin1D (const std::string& name);

    std::list<std::string> header;   // Header parser.
    bool                   inHeader; // Do we parse header.
    bool                   ok;       // Is accumulator in valid state.
    std::auto_ptr<TH1>     hist;     // Point to histogram being built. Initially null.
    bool                   norm;     // Should histogram bins be normalized
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

PBinInfo AccumHist::parseBinI() {
    int lo = parseString<int>(pop(), "# Low  = ");
    int hi = parseString<int>(pop(), "# High = ");
    return boost::make_shared<BinInfo>(
        (int)(hi - lo + 1),
        lo - 0.5   ,
        hi + 0.5   );
}

PBinInfo AccumHist::parseBinInt() {
    int base = parseString<int>(pop(), "# Base = ");
    int size = parseString<int>(pop(), "# Step = ");
    int n    = parseString<int>(pop(), "# Bins = ");
    return boost::make_shared<BinInfo>(
        n, base - 0.5*size, base + (n + 0.5) * size );
}

PBinInfo AccumHist::parseBinF() {
    double min  = parseString<double>(pop(), "# Base = ");
    double step = parseString<double>(pop(), "# Step = ");
    int    n    = parseString<int>(pop(),    "# N    = ");
    return boost::make_shared<BinInfo>(
        n, min, min + step*n );
}

PBinInfo AccumHist::parseBinLogD() {
    double lo = parseString<double>(pop(), "# Lo   = ");
    int    n  = parseString<int>   (pop(), "# N    = ");
    double hi = parseString<double>(pop(), "# Hi   = ");
    return boost::make_shared<BinInfo>(
        n, lo, hi, BinInfo::BinLog() );
}

PBinInfo AccumHist::parseBin1D(const std::string& name) {
    if       ( name == "# BinI"   ) {
        return parseBinI();
    } else if( name == "# BinInt" ) {
        return parseBinInt();
    } else if( name == "# BinF"   ) {
        return parseBinF();
    } else if( name == "# BinD"   ) {
        return parseBinF();
    } else if( name == "# LogBinD"   ) {
        return parseBinLogD();
    } else {
        throw ParseError("unknown bin type '" + name + "'");
    }
}

void AccumHist::parseHistogram1D(const std::string& name) {
    PBinInfo bin = parseBin1D(name);
    hist = std::auto_ptr<TH1>( allocHist1D( bin ) );
    norm = bin->shouldNormalize();
}

void AccumHist::parseHistogram2D() {
    parseString<void>(pop(), "# X");
    PBinInfo binX = parseBin1D(pop());
    parseString<void>(pop(), "# Y");
    PBinInfo binY = parseBin1D(pop());
    hist = std::auto_ptr<TH1>( allocHist2D( binX, binY ) );
    norm = binX->shouldNormalize() || binY->shouldNormalize();
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
                if( ok ) {
                    int    n = hist->FindBin( x );
                    if( norm ) {
                        double w = hist->GetBinWidth( n );
                        hist->SetBinContent( n, y / w );
                    } else {
                        hist->SetBinContent( n, y );
                    }
                }
            } else {
                double x,y,z;
                ok = 3 == sscanf(str.c_str(), "(%lf,%lf) %lf", &x, &y, &z);
                // EVIL. Kludgely reinterpret pointer type.
                TH2* h = dynamic_cast<TH2*>( hist.get() );
                if( h == 0 ) {
                    std::cerr << "rt-biplot: internal error. Histogram is not 2D\n";
                    return false;
                }
                if( ok ) {
                    // FIXME: normalization
                    h->Fill(x,y,z);
                }
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
