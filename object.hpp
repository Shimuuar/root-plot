#ifndef ROOT_PLOT__HPP__
#define ROOT_PLOT__HPP__

#include <vector>
#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>


// Axix range
struct Range {
    Range(double a, double b) :
        low(a), hi(b)
    {}
    double low;
    double hi;
};


class PlotObject;
class TLegend;

class Plot : public boost::noncopyable {
public:
private:
    std::vector< boost::shared_ptr<PlotObject> > objStack; // Stack of objects
    boost::optional<Range>                       xRange;   // X range
    boost::optional<Range>                       yRange;   // Y range
    boost::shared_ptr<TLegend>                   legend;   // Legend of the plot
};

// Object which
class PlotObject {
public:
    // Add object on a plot
    virtual void plotOn(Plot* cxt) = 0;

    // X range for object
    virtual boost::optional<Range> xRange() const = 0;
    // Y range for object
    virtual boost::optional<Range> yRange() const = 0;

    // Set color of line
    virtual void setLineColor();
    // Set width of line
    virtual void setLineWidth(int w);
};




class PlotHist : public PlotObject {
};

class PlotGraph : public PlotObject {
};

class VLine : public PlotObject {
};

class HLine : public PlotObject {
};

#endif /* ROOT_PLOT__HPP__ */
