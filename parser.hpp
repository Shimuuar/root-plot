
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
        parser(par),
        plot  (p),
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

// Accumulator for graphs
class AccumGraph : public LineAccum {
public:
    AccumGraph();
    virtual ~AccumGraph();
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);
protected:
    class Private;
    boost::scoped_ptr<Private> p;
};

// Accumulator for polygons
class AccumPoly : public AccumGraph {
public:
    AccumPoly();
    virtual ~AccumPoly();
    virtual bool flush(Plot*);
};

// Accumulator for barchart
class AccumBarchart : public AccumGraph {
public:
    AccumBarchart();
    virtual ~AccumBarchart();
    virtual bool flush(Plot*);
};

// Accumulator for histograms.
class AccumHist : public LineAccum {
public:
    AccumHist();
    virtual ~AccumHist();
    virtual bool flush(Plot*);
    virtual bool feedLine(const std::string& str);
private:
    class Private;
    boost::scoped_ptr<Private> p;
};


// ================================================================

// Line parser
class Parser {
public:
    // Construct parser
    Parser();

    // Feed line to the parser
    void feedLine(Plot* plot, const std::string& str);
    // Templated setter for accmulator
    template<typename T>
    void accumulate() { accum = boost::shared_ptr<T>( new T() ); }
    // Read data from file
    template<typename T>
    void readFromFile(const std::string& str, Plot* plot) {
        T acc;
        acc.readFromFile(str, plot);
    }
private:
    // Pointer to current accumulator
    boost::shared_ptr<LineAccum> accum;
};

#endif /* RT_ROOT_PARSER__HPP__ */
