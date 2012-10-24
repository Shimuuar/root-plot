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

class TObject;
class TCanvas;
class TLegend;
class TGraph;
class TGraph2D;
class TPolyLine;
class TH1;

// Abstracts over ROOT's canvas.
class Plot : public boost::noncopyable {
public:
    // Allowed values of color
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
    // Line orientation
    enum Line {
        Vertical,
        Horizontal
    };
    // Axis orientation
    enum Axis {
        X, Y, Z
    };
    // Style of line for graphs
    enum LineStyle {
        NoLine,
        SolidLine,
        Splines
    };
    // Marker style for graphs
    enum MarkerStyle {
        NoMarker   = 0,
        MarkerDot  = 1,
        MarkerPlus = 2,
        MarkerStar = 3,
        MarkerO    = 4,
        MarkerX    = 5,
    };
    // How error bars should be draws
    enum ErrorsStyle {
        NoErrors,
        Crosshairs,
        ErrorBand
    };

    // Convert int to color. Values which are out of range are
    // converted to black.
    static Color toColor(int c) {
        if( c < 0 || c > VIOLET )
            return BLACK;
        return Color(c);
    }

    // Construct plot object which will draw on the canvas. Plot
    // object doesn't own canvas.
    Plot(TCanvas* cnv);

    // Draw everything. This is slow call since it first remove
    // everything from canvas and then redraws every element in stack
    void draw(bool force = false);
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
    void setLineColor(int);
    // Set line style for top object. Noop if stack is empty
    void setLineStyle(Plot::LineStyle);
    // Set marker style for top object. Noop if stack is empty
    void setMarkerStyle(Plot::MarkerStyle);
    // Set error style
    void setErrorStyle(Plot::ErrorsStyle);
    // Set fill color for top object. Noop if stack is empty
    void setFillColor(int);
    // Set fill style for top object. Style number is chosen as ROOT
    //   * Negative - no filling.
    //   * 0        - solid (default).
    //   * 1-999    - Pattern using ROOT encoding.
    // Noop if stack is empty
    void setFillStyle(int);
    // Set line width for top object. Noop if stack is empty
    void setLineWidth(int width);

    // X range for plot
    RangeM xRange() const;
    // Y range for plot
    RangeM yRange() const;
    // Set range for axis
    void setRange(Axis axis, boost::optional<double> a, boost::optional<double> b);
    // Set range for axis to auto scale
    void setRange(Axis axis);

    // Remove legend from plot. Noop if there is no legend
    void removeLegend();
    // Add legend to the plot
    void addLegend(double x1, double y1, double x2, double y2);
    // Add string to a legend
    void addLegendString(const std::string& str);
    // Add string to a legend and link it with last object
    void addPlotToLegend(const std::string& str);

    // Set text drawing histograms
    void setHistText( bool txt );
    // Set scatter plot for 2D histogram
    void setHistScatter( bool scat );
    // Set contour plot for 2D histograms (negative to disable)
    void setHistContour( int n );
    // Set color plot for 2D histogram
    void setHistColor( bool c );
    // Set color plot for 2D histogram
    void setHistBox  ( bool b );
    // Set palette of and off. Have effect iff color option is on.
    void setHistPalette( bool p );
private:
    // Remove everything from canvas
    void clearCanvas();

    typedef std::vector< boost::shared_ptr<PlotObject> > Stack;

    // Data
    TCanvas*                     m_canvas;    // Canvas to draw on
    Stack                        m_objStack;  // Stack of objects
    // Axis
    boost::optional<double>      m_xLow;      // X low range
    boost::optional<double>      m_xHi;       // X hi range
    boost::optional<double>      m_yLow;      // Y low range
    boost::optional<double>      m_yHi;       // Y hi range
    boost::optional<std::string> m_xLabel;    // X axis label
    boost::optional<std::string> m_yLabel;    // Y axis label
    bool                         m_xLog;      // Log scale for X axis
    bool                         m_yLog;      // Log scale for Y axis
    bool                         m_zLog;      // Log scale for Z axis
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

    // X range for object. Object should return minimal range in which
    // in could be fitted.
    virtual RangeM xRange() const = 0;
    // Y range for object. Same as X range.
    virtual RangeM yRange() const = 0;

    // Set color of line
    virtual void setLineColor(int) {}
    // Set line style. Used for graphs
    virtual void setLineStyle(Plot::LineStyle) {}
    // Set marker style
    virtual void setMarkerStyle(Plot::MarkerStyle) {}
    // Set error style
    virtual void setErrorStyle(Plot::ErrorsStyle) {}
    // Set width of line
    virtual void setLineWidth(int width)   {UNUSED(width);}
    // Set fill color
    virtual void setFillColor(int) {}
    // Set fill style. It accepts ROOT fill style. Conversions are
    // handled by Plot::setFillStyle.
    virtual void setFillStyle(int) {}

    // Set text drawing histograms
    virtual void setHistText   ( bool txt )  {UNUSED(txt);}
    // Set scatter plot for 2D histogram
    virtual void setHistScatter( bool scat ) {UNUSED(scat);}
    // Set contour plot for 2D histograms (negative to disable)
    virtual void setHistContour( int n )     {UNUSED(n);}
    // Set color plot for 2D histogram
    virtual void setHistColor  ( bool c )    {UNUSED(c);}
    // Set color plot for 2D histogram
    virtual void setHistBox    ( bool b )    {UNUSED(b);}
    // Set palette of and off. Have effect iff color option is on
    virtual void setHistPalette( bool p )    {UNUSED(p);}

    // Get pointer to ROOT object. May return NULL
    virtual TObject* getRootObject() { return 0; }
};



// Wrapper around ROOT histograms
class PlotHist : public PlotObject {
public:
    // Create graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotHist(TH1* h);

    virtual void     plotOn(Plot* cxt);
    virtual RangeM   xRange() const;
    virtual RangeM   yRange() const;
    virtual void     setLineWidth(int width);
    virtual void     setLineColor(int);
    virtual void     setFillColor(int);
    virtual void     setFillStyle(int);
    virtual TObject* getRootObject();

    virtual void setHistText( bool txt )     { m_text    = txt;  }
    virtual void setHistScatter( bool scat ) { m_scatter = scat; }
    virtual void setHistContour( int n )     { m_nCont   = n;    }
    virtual void setHistColor( bool c )      { m_color   = c;    }
    virtual void setHistBox  ( bool b )      { m_box     = b;    }
    virtual void setHistPalette( bool p )    { m_palette = p;    }

private:
    boost::scoped_ptr<TH1> hist; // Histogram
    // Common flags
    int  m_lineWidth;            // Line width
    bool m_text;                 // Draw text at nodes?
    // 2D histogram
    bool m_scatter;              // Draw as scatter plot
    bool m_box;                  // Draw as box plot
    int  m_nCont;                // Number of contours. Negative for no contours
    bool m_color;                // Color plot
    bool m_palette;              // Pallette
};

// Wrapper around ROOT graphs
class PlotGraph : public PlotObject {
public:
    // Create graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotGraph(TGraph* g);

    virtual void     plotOn(Plot* cxt);
    virtual RangeM   xRange() const;
    virtual RangeM   yRange() const;
    virtual void     setLineWidth(int width);
    virtual void     setLineStyle(Plot::LineStyle);
    virtual void     setMarkerStyle(Plot::MarkerStyle);
    virtual void     setErrorStyle(Plot::ErrorsStyle);
    virtual void     setLineColor(int);
    virtual void     setFillColor(int);
    virtual void     setFillStyle(int);
    virtual TObject* getRootObject();
protected:
    int               color;
    Plot::LineStyle   line;
    Plot::MarkerStyle marker;
    Plot::ErrorsStyle errs;
    boost::scoped_ptr<TGraph>  graph;
    boost::shared_ptr<TObject> clone;
};

class PlotGraph2D : public PlotObject {
public:
    // Create 2D graph out of ROOT graph. Will take outnership of the
    // copy.
    PlotGraph2D(TGraph2D* g);

    virtual void     plotOn(Plot* cxt);
    virtual RangeM   xRange() const;
    virtual RangeM   yRange() const;
    virtual TObject* getRootObject();
protected:
    boost::scoped_ptr<TGraph2D> graph;
};

// Decorator for bar charts
class PlotBarChart : public PlotGraph {
public:
    PlotBarChart(TGraph* g);

    virtual void     plotOn(Plot* cxt);
    virtual RangeM   xRange() const;
    virtual RangeM   yRange() const;
    virtual void     setFillColor(int);
    virtual void     setFillStyle(int);
};

// Wrapper around ROOT polygons
class PlotPoly : public PlotObject {
public:
    // Create graph out of ROOT TPolyLine. Will take outnership of the
    // copy.
    PlotPoly(TPolyLine* g);

    virtual void     plotOn(Plot* cxt);
    virtual RangeM   xRange() const;
    virtual RangeM   yRange() const;
    virtual void     setLineWidth(int width);
    virtual void     setLineColor(int);
    virtual void     setFillColor(int);
    virtual void     setFillStyle(int);
    virtual TObject* getRootObject();
private:
    int width;
    boost::scoped_ptr<TPolyLine> poly;
};


// Vertical or horizontal line on the plot. Ranges are adjusted
// automatically
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
    virtual void   setLineColor(int);
private:
    Plot::Line orientation;
    double     x;
    int        color;
    int        width;

    boost::shared_ptr<TGraph> graph;
};

// Vertical or horizontal line on the plot. Ranges are adjusted
// automatically
class PlotBand : public PlotObject {
public:
    // Create vertical line at position x
    PlotBand(Plot::Line orientation_, double x1_, double x2_) :
        orientation(orientation_),
        x1   (x1_ < x2 ? x1_ : x2_),
        x2   (x1_ < x2 ? x2_ : x1_),
        width( 0     ),
        color( Plot::BLACK ),
        fill ( 20    ),
        fillStyle( 1001 )
    {}

    virtual ~PlotBand() {}
    virtual void   plotOn(Plot* cxt);
    virtual RangeM xRange() const;
    virtual RangeM yRange() const;
    virtual void   setFillColor(int);
    virtual void   setFillStyle(int);
    virtual void   setLineWidth(int width);
    virtual void   setLineColor(int);
private:
    Plot::Line orientation;
    double     x1,x2;
    int        width, color, fill, fillStyle;

    boost::shared_ptr<TPolyLine> poly;
};

#endif /* RT_ROOT_PLOT__HPP__ */
