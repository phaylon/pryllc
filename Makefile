PROGRAM 	= bin/pryll
SCM_FILES 	= $(shell find lib/ -type f -name "*.scm")
OBJS 		= $(patsubst %.scm, %.o, $(SCM_FILES))
BUILDSCM	= blib/build.scm
BUILDOBJ	= blib/build.o
MAINSCM		= bin/pryll.scm
MAINOBJ		= bin/pryll.o

all: $(PROGRAM)

rebuild: clean all

clean:
	rm -f $(OBJS)
	rm -f $(PROGRAM)
#	rm -f $(BUILDSCM)
#	rm -f $(BUILDOBJ)
	rm -f $(MAINOBJ)
#	rmdir blib

test: force
	csi -s t/lexing.t.scm
	csi -s t/compilation.t.scm

$(PROGRAM): $(MAINOBJ) $(OBJS)
#	@if [ ! -e blib ]; then mkdir blib; fi
#	@echo\
#		"(declare (unit build))\n"\
#		"(define build/libfiles\n"\
#		"  (list"\
#		$(patsubst %, '"%"', $(OBJS))\
#		'"'$(BUILDOBJ)'"'\
#		"))"\
#		> $(BUILDSCM)
#	csc -c $(BUILDSCM)
	csc $(OBJS) $(MAINOBJ) -o $(PROGRAM)

$(MAINOBJ): $(MAINSCM)
	csc -c $(MAINSCM)

lib/%.o: lib/%.scm
	csc -c $<

force: ;
