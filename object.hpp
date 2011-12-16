#ifndef RT_ROOT_PLOT__HPP__
#define RT_ROOT_PLOT__HPP__

// Classes for plotting

#include <string>
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

// Abstracts over ROOT's canvas. 
class Plot : public boost::noncopyable {
public:
    enum Color {
        WHITE   = 0,
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
    // Convert int to color. Values which are out of range are
    // converted to black.
    static Color toColor(int c) {
        if( c < 0 || c > VIOLET )
            return BLACK;
        return Color(c);
    }

    // Line orientation
    enum Line {
        Vertical,
        Horizontal
    };
    // Axis
    enum Axis {
        X, Y
    };

    // Construct plot object which will draw on the canvas. Plot
    // object doesn't own canvas.
    Plot(TCanvas* cnv);
    
    // Draw everything. This is slow call since it first remove
    // everything from canvas and then redraws every element in stack
    void draw();
    // Try to save plot into file
    void save(const std::string& str);

    // ======================================== //
    // Object manipulations

    // Clear plot and delete entire object stack
    void clear();
    // Set title of the plot
    void setTitle(const std::string& title) { m_title = title; }
    // Set silent mode on/off. In silent mode canvas is not updated.
    void setSilent(bool isSilent) { m_isSilent = isSilent; }
    // Set label for axis
    void setLabel(Axis axis, const std::string& label);
    // Set log scale
    void setLogScale(Axis axis, bool l);

    // Push object on the top of stack;
    void pushObject(boost::shared_ptr<PlotObject> plot);
    // Set line color for top object. Noop if stack is empty
    void setLineColor(Plot::Color);
    // Set fill color for top object. Noop if stack is empty
    void setFillColor(Plot::Color);
    // Set line width for top object. Noop if stack is empty
    void setLineWidth(int width);
    
    // X range for plot
    RangeM xRange() const;
    // Y range for plot
    RangeM yRange() const;
    // Set range for axis
    void setRange(Axis axis, double a, double b);
    // Set range for axis to auto scale
    void setRange(Axis axis);

    // Remove legend from plot. Noop if there is no legend
    void removeLegend();
    // Add legend to the plot
    void addLegend(double x1, double y1, double x2, double y2);
private:
    // Remove everything from canvas
    void clearCanvas();
    
    typedef std::vector< boost::shared_ptr<PlotObject> > Stack;

    // Data
    TCanvas*                     m_canvas;    // Canvas to draw on
    Stack                        m_objStack;  // Stack of objects
    // Axis
    boost::optional<Range>       m_xRange;    // X range
    boost::optional<Range>       m_yRange;    // Y range
    boost::optional<std::string> m_xLabel;    // X axis label
    boost::optional<std::string> m_yLabel;    // Y axis label
    bool                         m_xLog;      // Log scale for X axis
    bool                         m_yLog;      // Log scale for Y axis
    // Whole plot
    boost::shared_ptr<TLegend>   m_legend;    // Legend of the plot
    bool                         m_isSilent;  // Is silent mode on
    std::string                  m_title;     // Title of plot
    boost::shared_ptr<TGraph>    m_axisGraph; // Graph which holds axis.
};

// Object which
class PlotObject {
public:
    virtual ~PlotObject() {}
    
    // Draw object on the plot. This function depends on correct
    // values of ROOT global variables (current dir) and correct
    // canvas setup by Plot. and should be invoked from Plot only.
    // 
    // Plot* plot  - plot to draw on
    // bool  first - Whether object is first on the plot or not.
    virtual void plotOn(Plot* cxt) = 0;

    // X range for object
    virtual RangeM xRange() const = 0;
    // Y range for object
    virtual RangeM yRange() const = 0;

    // Set color of line
    virtual void setLineColor(Plot::Color) {}
    // Set fill color
    virtual void setFillColor(Plot::Color) {}
    // Set width of line
    virtual void setLineWidth(int width)   {UNUSED(width);}
};



// Wrapper around ROOT histograms
class PlotHist : public PlotObject {
public:
    // Create graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotHist(TH1* h);

    virtual void   plotOn(Plot* cxt);
    virtual RangeM xRange() const;
    virtual RangeM yRange() const;
    virtual void   setLineWidth(int width);
    virtual void   setLineColor(Plot::Color);
    virtual void   setFillColor(Plot::Color);
private:
    boost::scoped_ptr<TH1> hist;
};

// Wrapper around ROOT graphs
class PlotGraph : public PlotObject {
public:
    // Create graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotGraph(TGraph* g);

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
    PlotLine(Plot::Line orientation_, double x_) :
        orientation(orientation_),
        x(x_),
        color(Plot::BLACK),
        width(1)
    {}

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
    
    boost::shared_ptr<TGraph> graph;
};

#endif /* RT_ROOT_PLOT__HPP__ */
