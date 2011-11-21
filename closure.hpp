#ifndef RT_PLOT_CLOSURE__HPP__
#define RT_PLOT_CLOSURE__HPP__

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <vector>

// Poor man's closures for C++

// Base class for closures
template<typename type>
class Closure : public boost::noncopyable {
public:
    // Shared pointer to closure
    typedef boost::shared_ptr<Closure> ptr;
    
    virtual ~Closure() {}
    virtual void force(type*) = 0;
};

template<typename type>
class Closure0P : public Closure<type> {
public:
    typedef void (type::*func)();
    
    Closure0P(func fun) :
        f(fun)
    {}
    virtual ~Closure0P() {}
    virtual void force(type* p) {
        (p->*f)();
    }
private:
    func f;
};

template<typename type, typename T1>
class Closure1P : public Closure<type> {
public:
    typedef void (type::*func)(T1);
    
    Closure1P(func fun, const T1& a) :
        f(fun),x1(a)
    {}
    virtual ~Closure1P() {}
    virtual void force(type* p) {
        (p->*f)(x1);
    }
private:
    func f;
    T1   x1;
};

template<typename type, typename T1, typename T2>
class Closure2P : public Closure<type> {
public:
    typedef void (type::*func)(T1,T2);
    Closure2P(func fun, const T1& a, const T2& b) :
        f(fun), x1(a), x2(b)
    {}
    virtual ~Closure2P() {}
    virtual void force(Plot* p) {
        (p->*f)(x1,x2);
    }
private:
    func f;
    T1 x1;
    T2 x2;
};
 

// Convenience functions
template<typename type>
inline typename Closure<type>::ptr
delay( typename Closure0P<type>::func f ) {
    return boost::shared_ptr< Closure<type> >( new Closure0P<type>(f) );
}

template<typename type, typename T1>
inline typename Closure<type>::ptr
delay( typename Closure1P<type,T1>::func f, const T1& a ) {
    return boost::shared_ptr< Closure<type> >( new Closure1P<type,T1>(f, a) );
}

template<typename type, typename T1, typename T2>
inline typename Closure<type>::ptr
delay( typename Closure2P<type,T1,T2>::func f, const T2& a ) {
    return boost::shared_ptr< Closure<type> >( new Closure2P<type,T1,T2>(f, a) );
}


// Stack of closures
template<typename type>
class ClosureStack : public Closure<type> {
public:
    virtual ~ClosureStack() {}

    void append( typename Closure<type>::ptr x ) {
        closures.push_back( x );
    }

    // Delay evaluation
    void delay( typename Closure0P<type>::func f) {
        append( ::delay<type>(f) );
    }
    template<typename T1>
    void delay( typename Closure1P<type,T1>::func f, const T1& x) {
        append( ::delay<type,T1>(f,x) );
    }
    
    virtual void force(type* p) {
        for(size_t i = 0; i < closures.size(); i++ ) {
            closures[i]->force();
        }
    }
private:
    std::vector< typename Closure<type>::ptr > closures;
};

#endif /* RT_PLOT_CLOSURE__HPP__ */
