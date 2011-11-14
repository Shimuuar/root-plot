#ifndef ROOT_PLOT__HPP__
#define ROOT_PLOT__HPP__

// Classes for plotting


#include <vector>
#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/noncopyable.hpp>

#define UNUSED(x) do{ (void)(x); }while(0)

// ================================================================ //
// == Small types
// ================================================================ //

// Axix range
struct Range {
    Range(double a, double b) :
        low(a), hi(b)
    {}
    double low;
    double hi;
};

typedef boost::optional<Range> RangeM;

// ================================================================ //
// == Plot types
// ================================================================ //

class PlotObject;

class TCanvas;
class TLegend;
class TGraph;
class TH1;

class Plot : public boost::noncopyable {
public:
    enum Color {
        BLACK   = 1,
        RED     = 2,
        GREEN   = 3,
        BLUE    = 4,
        YELLOW  = 5,
        MAGENTA = 6,
        CYAN    = 7,
        FOREST  = 8,
        VIOLET  = 9
    };
    enum Line {
        Vertical,
        Horizontal
    };

    // Clear plot and delete entire object stack
    void clear();
    
    // Draw everything
    void draw();
    // Redraw. Just makes canvas update
    void redraw();

    // TCanvas* getCanvas() { return cnv; }
private:
    TCanvas*                                     canvas;
    std::vector< boost::shared_ptr<PlotObject> > objStack; // Stack of objects
    boost::optional<Range>                       xRange;   // X range
    boost::optional<Range>                       yRange;   // Y range
    boost::shared_ptr<TLegend>                   legend;   // Legend of the plot
};

// Object which
class PlotObject {
public:
    virtual ~PlotObject() {}
    
    // Add object on a plot
    virtual void plotOn(Plot* cxt) = 0;

    // X range for object
    virtual RangeM xRange() const = 0;
    // Y range for object
    virtual RangeM yRange() const = 0;

    // Set color of line
    virtual void setLineColor(Plot::Color) {}
    // Set width of line
    virtual void setLineWidth(int width)   {UNUSED(width);}
};




class PlotHist : public PlotObject {
    // Create graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotHist(TGraph* graph);

    virtual void   plotOn(Plot* cxt);
    virtual RangeM xRange() const;
    virtual RangeM yRange() const;
    virtual void   setLineWidth(int width);
    virtual void   setLineColor(Plot::Color);
private:
    boost::scoped_ptr<TH1> hist;
};

class PlotGraph : public PlotObject {
public:
    // Create graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotGraph(TGraph* graph);

    virtual void   plotOn(Plot* cxt);
    virtual RangeM xRange() const;
    virtual RangeM yRange() const;
    virtual void   setLineWidth(int width);
    virtual void   setLineColor(Plot::Color);
private:
    boost::scoped_ptr<TGraph> graph;
};


// Line of the plot. Ranges are determined automatically
class PlotLine : public PlotObject {
public:
    // Create vertical line at position x
    PlotLine(Plot::Line orientation, double x);

    virtual ~PlotLine() {}
    virtual void   plotOn(Plot* cxt);
    virtual RangeM xRange() const;
    virtual RangeM yRange() const;
    virtual void   setLineWidth(int width);
    virtual void   setLineColor(Plot::Color);
private:
    Plot::Line orientation;
    double     x;
    int        color;
    int        width;
};

#endif /* ROOT_PLOT__HPP__ */