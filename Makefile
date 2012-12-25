
.PHONY : all clean install install-c install-haskell uninstall 

# Check for ROOTSYS 
ifeq ($(ROOTSYS),)
  ROOTCFG  = root-config
  ROOTCINT = rootcint
else
  ROOTCFG  = ${ROOTSYS}/bin/root-config
  ROOTCINT = ${ROOTSYS}/bin/rootcint
endif 


INSTALLDIR=${HOME}/opt
BINDIR    =${INSTALLDIR}/bin
LIBDIR    =${INSTALLDIR}/lib
INCLDIR   =${INSTALLDIR}/include

################################################################
# Strict aliasing is turned off since it triggers warning for
# boost::optional. It maybe unjustified but it's better to be on safe
# side.

C        = gcc
CXX      = g++
CXXFLAGS = -g -O2 -fno-strict-aliasing  -Wall -Wextra `${ROOTCFG} --cflags`
CFLAGS   = -g -O2 -Wall -Wextra -std=c99
LDFLAGS  = `${ROOTCFG} --libs` -lGui -lfl
CLDFLAGS = -L. -lrt-plot
PREFIX   = ${HOME}/opt
HEADERS  = object.hpp reader.hpp parser.hpp exceptions.hpp \
	   RtMainFrame.hpp RtPlot.hpp
OBJS     = \
	main.o object.o reader.o histogram.o    \
	parser.o parser.l.o parser.y.o          \
	RtPlot.o      RtPlot-cint.o             \
	RtMainFrame.o RtMainFrame-cint.o
EXE      = rt-plot rt-listen rt-cat
LIBS     = librt-plot.a


################################################################
all : ${EXE}

librt-plot.a : socket.o
	ar -cvq $@ $^

rt-plot : ${OBJS}
	${CXX} ${CXXFLAGS} $^ -o $@ ${LDFLAGS}
rt-listen: rt-listen.o  librt-plot.a
	${C} ${CFLAGS} $< -o $@ ${CLDFLAGS}
rt-cat: rt-cat.o librt-plot.a
	${C} ${CFLAGS} $< -o $@ ${CLDFLAGS}

# Add CINT classes.
%-cint.h %-cint.cpp: %.hpp
	rm -f $@ `echo $@ | sed s/cpp$$/h/`
	${ROOTCINT} $@ -c "$<-"

%.o : %.cpp $(HEADERS)
	${CXX} -c ${CXXFLAGS} $< -o $@
%.o : %.c 
	${C} ${CFLAGS} -c $< -o $@
parser.l.cpp : parser.l parser.y.cpp
	flex --outfile=$@ --header=parser.l.hpp $<
parser.y.cpp : parser.y
	bison --defines=parser.tab.h parser.y -o parser.y.cpp
# Extra deps
parser.o : parser.l.cpp


clean:
	rm -rf *.o *.a rt-biplot *-cint.cpp *-cint.h *.y.* *.l.* *.tab.h ${EXE}
# Installation
install: all
	mkdir -p ${LIBDIR}
	mkdir -p ${BINDIR}
	mkdir -p ${INCLDIR}
	install rt-plot      ${BINDIR}
	install rt-listen    ${BINDIR}
	install rt-cat       ${BINDIR}
	install rt-tree      ${BINDIR}
	install rt-shell     ${BINDIR}
	install librt-plot.a ${LIBDIR}
	install rt-plot.h    ${INCLDIR}
