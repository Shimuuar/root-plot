
.PHONY : all clean install uninstall 

# Check for ROOTSYS 
ifeq ($(ROOTSYS),)
  ROOTCFG = root-config
else
  ROOTCFG = ${ROOTSYS}/bin/root-config
endif 


INSTALLDIR=${HOME}/opt/bin
################################################################

C        = gcc
CXX      = g++
CXXFLAGS = -g -O2 -Wall -Wextra `${ROOTCFG} --cflags`
CFLAGS   = -g -O2 -Wall -Wextra -std=c99
LDFLAGS  = `${ROOTCFG} --libs` -lGui -lfl
PREFIX   = ${HOME}/opt
HEADERS  = object.hpp reader.hpp parser.hpp
#BiplotMainFrame.hpp  hist.hpp  parser.hpp  plotobj.hpp  ptr.hpp

################################################################
all : rt-plot


rt-plot : main.o object.o reader.o parser.o parser.lex.o
	${CXX} ${CXXFLAGS} $^ -o $@ ${LDFLAGS}


# Add CINT classes.
#BiplotMainFrame-cint.cpp: BiplotMainFrame.hpp
#	rm -f rt-biplot-cint.*#
#	${ROOTSYS}/bin/rootcint $@ -c BiplotMainFrame.hpp

%.o : %.cpp $(HEADERS)
	${CXX} -c ${CXXFLAGS} $< -o $@
%.o : %.c 
	${C} ${CFLAGS} -c $< -o $@
%.lex.cpp : %.lex
	flex -o $@ $<


# rt-echo : rt-echo.o
# 	${C} ${CFLAGS} -o $@ $+
# rt-unix-wrapper : rt-unix-wrapper.o
# 	${C} ${CFLAGS} -o $@ $+
# rt-biplot: rt-biplot.o BiplotMainFrame.o BiplotMainFrame-cint.o hist.o plotobj.o parser.o
# 	${CXX} ${CXXFLAGS} $^ -o $@ ${LDFLAGS}

clean:
	rm -rf *.o rt-biplot *-cint.cpp *-cint.h rt-plot
# install: all 
# 	mkdir -p ${INSTALLDIR}
# 	install rt-biplot ${INSTALLDIR}
# 	install rt-shell  ${INSTALLDIR}
# 	install rt-server ${INSTALLDIR}
# #	install rt-tree   ${INSTALLDIR}
# 	install rt-echo         ${INSTALLDIR}
# 	install rt-unix-wrapper ${INSTALLDIR}
