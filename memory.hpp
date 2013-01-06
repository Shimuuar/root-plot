#ifndef RT_MEMORY_HPP
#define RT_MEMORY_HPP

/*
 * rt-plot manages lifetime of all explicitly created ROOT
 * objects. Because of brain-damaged model of ownership we have to set
 * certain bits for object. It's error prone to add them manually so
 * following methods should be used used
 */
#include <boost/make_shared.hpp>
#include <TObject.h>


namespace detail {
    template<typename T>
    inline void addRootFlags( T p ) {
        p->ResetBit( kCanDelete   );
        p->SetBit  ( kMustCleanup );        
    }
}

// ----------------------------------------------------------------
// Allocate ROOT objects as raw pointers
// ----------------------------------------------------------------

template <typename T>
inline T* newROOT() {
    T* p = new T;
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1>
inline T* newROOT(A1 a1) {
    T* p = new T(a1);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2>
inline T* newROOT(A1 a1, A2 a2) {
    T* p = new T(a1,a2);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2, typename A3>
inline T* newROOT(A1 a1, A2 a2, A3 a3) {
    T* p = new T(a1,a2,a3);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2, typename A3, typename A4>
inline T* newROOT(A1 a1, A2 a2, A3 a3, A4 a4) {
    T* p = new T(a1,a2,a3,a4);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5>
inline T* newROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5) {
    T* p = new T(a1,a2,a3,a4,a5);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
inline T* newROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6) {
    T* p = new T(a1,a2,a3,a4,a5,a6);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
inline T* newROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7) {
    T* p = new T(a1,a2,a3,a4,a5,a6,a7);
    detail::addRootFlags(p);
    return p;
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
inline T* newROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8) {
    T* p = new T(a1,a2,a3,a4,a5,a6,a7,a8);
    detail::addRootFlags(p);
    return p;
}



// ----------------------------------------------------------------
// Allocate ROOT objects are shared_ptr
// ----------------------------------------------------------------

template <typename T>
inline boost::shared_ptr<T> makeROOT() {
    return boost::shared_ptr<T>( newROOT<T>() );
}

template <typename T, typename A1>
inline boost::shared_ptr<T> makeROOT(A1 a1) {
    return boost::shared_ptr<T>( newROOT<T>(a1) );
}

template <typename T, typename A1, typename A2>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2) );
}

template <typename T, typename A1, typename A2, typename A3>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2, A3 a3) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2,a3) );
}

template <typename T, typename A1, typename A2, typename A3, typename A4>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2, A3 a3, A4 a4) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2,a3,a4) );
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2,a3,a4,a5) );
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2,a3,a4,a5,a6) );
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2,a3,a4,a5,a6,a7) );
}

template <typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
inline boost::shared_ptr<T> makeROOT(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8) {
    return boost::shared_ptr<T>( newROOT<T>(a1,a2,a3,a4,a5,a6,a7,a8) );
}

#endif /* RT_MEMORY_HPP */
