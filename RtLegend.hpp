#ifndef RT_LEGEND
#define RT_LEGEND

#include <boost/shared_ptr.hpp>
#include <TPave.h>

class RtLegend : public TPave {
public:
    RtLegend(double x1, double y1, double x2, double y2);
    
    void addEntry(boost::shared_ptr<TObject> o, const std::string& str);
    void addEntry(const std::string& str);
    void addEntry(const std::string& key, const std::string& val);
    
    // ========================================
    // Overloaded method.
    virtual ~RtLegend();
    virtual void Copy( TObject& o );
    virtual void Draw (Option_t* option = "");
    virtual void Clear(Option_t* option = ""); // *MENU*
    virtual void Paint(Option_t* option = "");
private:
    // Convert to coordinates used for drawing
    double drawX(double x, bool forceLinear = false);
    double drawY(double y, bool forceLinear = false);
    class Entry;

    // Coordinates
    double m_X1, m_X2;
    double m_Y1, m_Y2;
    // List of pointers to the entry
    std::vector<boost::shared_ptr<Entry> > entries;
};

#endif /* RT_LEGEND */
