
#ifndef RT_ROOT_PARSER__HPP__
#define RT_ROOT_PARSER__HPP__

#include <string>
#include <vector>
#include <iostream>

#include <boost/variant.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_ptr.hpp>



// ================================================================

class Plot;
class Parser;

// Token of the language
typedef boost::variant<int, double, std::string> Token;
// Define token type for bison
#define YYSTYPE Token

// Parameters for bison parser
struct ParseParam {
    ParseParam(Parser* par, Plot* p) :
        parser   (par),
        plot     (p),
        clearPlot(false),
        axis     (0),
        onOff    (true)
    {}
    
    Parser*    parser;          // Pointer to parser
    Plot*      plot;            // Pointer to plot object
    // ==== Locals ================
    bool       clearPlot;       // Should we clear plot
    int        axis;            // Axis
    bool       onOff;           // ON/OFF flag
};
int yyparse(ParseParam);



// ================================================================

// Incremental parser for embedded data. E.g. graphs, histograms etc.
class LineAccum {
public:
    virtual ~LineAccum() {}

    // If accumulator is ready it adds its contents to the plot and
    // returns true. Otherwise just returns false.
    virtual bool flush(Plot* plot) = 0;
    // Feed line to accumulator. If line is parsed sucesfully returns
    // true
    virtual bool feedLine(const std::string& str) = 0;
    // Reads from data from file line by list name and flushes result
    // into plot if succeeds.
    bool readFromFile(const std::string& fname, Plot* plot);
};

// Pointer to the line accumulator. It's always used in shared_ptr so
// it's possible to allocate.
typedef boost::shared_ptr<LineAccum> PLineAccum;



// Allocate accumulator for graphs
PLineAccum makeAccumGraph();

// Allocate accumulator for 2D graphs
PLineAccum makeAccumGraph2D();

// Allocate accumulator for polygons
PLineAccum makeAccumPoly();

// Allocate accumulator for barchart
PLineAccum makeAccumBarchart();

// Allocate accumulator for polygons
PLineAccum makeAccumHist();

// Allocate null accumulator. It does nothing
PLineAccum makeNullAccum();



// ================================================================

// Line parser. It's main parser class and it drops to the LineAccum
// when needed.
class Parser {
public:
    // Construct parser
    Parser();

    // Feed line to the parser
    void feedLine(Plot* plot, const std::string& str);
    // Templated setter for accumulator
    void accumulate(PLineAccum a) { m_accum = a; }
private:
    // Pointer to current accumulator
    boost::shared_ptr<LineAccum> m_accum;
};

#endif /* RT_ROOT_PARSER__HPP__ */
