PROGRAM = bin/pryll
SCM_FILES = $(wildcard lib/*.scm)
OBJS = $(patsubst %.scm, %.o, $(SCM_FILES))

all: $(PROGRAM)

clean:
	rm $(OBJS)
	rm $(PROGRAM)

test: force
	csi -s t/lexing.t.scm
	csi -s t/compilation.t.scm

$(PROGRAM): $(OBJS)
	csc $(OBJS) -o $(PROGRAM)

%.o: %.scm
	csc -c $<

force: ;
