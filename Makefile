
.PHONY : all clean install uninstall 

# Check for ROOTSYS 
ifeq ($(ROOTSYS),)
  ROOTCFG  = root-config
  ROOTCINT = rootcint
else
  ROOTCFG  = ${ROOTSYS}/bin/root-config
  ROOTCINT = ${ROOTSYS}/bin/rootcint
endif 


INSTALLDIR=${HOME}/opt/bin
################################################################

C        = gcc
CXX      = g++
CXXFLAGS = -g -O2 -Wall -Wextra `${ROOTCFG} --cflags`
CFLAGS   = -g -O2 -Wall -Wextra -std=c99
LDFLAGS  = `${ROOTCFG} --libs` -lGui -lfl
PREFIX   = ${HOME}/opt
HEADERS  = object.hpp reader.hpp parser.hpp exceptions.hpp \
	   RtMainFrame.hpp RtPlot.hpp
OBJS     = \
	main.o object.o reader.o histogram.o    \
	parser.o parser.l.o parser.y.o          \
	RtPlot.o      RtPlot-cint.o             \
	RtMainFrame.o RtMainFrame-cint.o


################################################################
all : rt-plot rt-listen rt-cat

rt-plot : ${OBJS}
	${CXX} ${CXXFLAGS} $^ -o $@ ${LDFLAGS}
rt-listen: rt-listen.o socket.o
	${C} ${CFLAGS} $^ -o $@
rt-cat: rt-cat.o socket.o
	${C} ${CFLAGS} $^ -o $@

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
	rm -rf *.o rt-biplot *-cint.cpp *-cint.h *.y.* *.l.* *.tab.h rt-plot 
install: all 
	mkdir -p ${INSTALLDIR}
	install rt-plot    ${INSTALLDIR}
	install rt-listen  ${INSTALLDIR}
	install rt-cat     ${INSTALLDIR}
	install rt-tree   ${INSTALLDIR}
