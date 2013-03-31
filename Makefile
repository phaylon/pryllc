PROGRAM 	= bin/pryll
DBGPROGRAM	= bin/prylldbg
SCM_FILES 	= $(shell find lib/ -type f -name "*.scm")
OBJS 		= $(patsubst %.scm, %.o, $(SCM_FILES))
#BUILDSCM	= blib/build.scm
#BUILDOBJ	= blib/build.o
MAINSCM		= bin/pryll.scm
MAINOBJ		= bin/pryll.o
DBGSCM		= bin/prylldbg.scm
DBGOBJ		= bin/prylldbg.o
GRAMMARSCM  = lib/grammar.scm.gen
GRAMMARYY   = lib/grammar.scm.yy

all: $(PROGRAM)

debug: $(PROGRAM) $(DBGPROGRAM)

rebuild: clean all

clean:
	rm -f $(OBJS)
	rm -f $(PROGRAM)
	rm -f $(DBGPROGRAM)
#	rm -f $(BUILDSCM)
#	rm -f $(BUILDOBJ)
	rm -f $(MAINOBJ)
	rm -f $(DBGOBJ)
	rm -f .pryll.*
	rm -f $(GRAMMARYY)
#	rmdir blib

test: force
	csi -s t/lexing.t.scm
	csi -s t/compilation.t.scm

$(GRAMMARYY): $(GRAMMARSCM)
	csi -s $(GRAMMARSCM)
	@rm -f lib/parser.o

$(DBGPROGRAM): $(GRAMMARYY) $(DBGOBJ) $(OBJS)
	csc $(OBJS) $(DBGOBJ) -o $(DBGPROGRAM)

$(PROGRAM): $(GRAMMARYY) $(MAINOBJ) $(OBJS)
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

$(DBGOBJ): $(DBGSCM)
	csc -c $(DBGSCM)

lib/%.o: lib/%.scm
	csc -c $<

force: ;
