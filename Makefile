RM   = rm -rf
ECHO = echo

EXAMPLES = \
 java-API     \
 acvc         \
 animate      \
 connect_four \
 hello        \
 jrapid       \
 mandelbrot   \
 philosophers \
 prime	      \
 quicksort    \
 text_io      \
 tictactoe    \
 waypoints

all :
	@$(ECHO) "type \"make run\" to run all the examples"

build :
	@for dir in $(EXAMPLES)*; do                 \
	    $(ECHO) building $$dir ...;              \
	    $(MAKE) -C $$dir build;                  \
	    $(ECHO);                                 \
	    $(ECHO);                                 \
	done

clean :
	$(RM) CVS
	@for dir in $(EXAMPLES)*; do                 \
	    $(ECHO) cleaning $$dir ...;              \
	    $(MAKE) -C $$dir RM="$(RM)" clean;       \
	    $(ECHO);                                 \
	    $(ECHO);                                 \
	done

distclean :
	$(RM) CVS
	@for dir in $(EXAMPLES)*; do                 \
	    $(ECHO) distcleaning $$dir ...;          \
	    $(MAKE) -C $$dir RM="$(RM)" distclean;   \
	    $(ECHO);                                 \
	    $(ECHO);                                 \
	done

flip :
	$(FLIP) *.txt Makefile
	$(ECHO)
	$(ECHO)
	@for dir in $(EXAMPLES)*; do                 \
	    $(ECHO) flipping $$dir ...;              \
	    $(MAKE) -C $$dir FLIP="$(FLIP)" flip;    \
	    $(ECHO);                                 \
	    $(ECHO);                                 \
	done

run :
	@for dir in $(EXAMPLES)*; do                 \
	    $(ECHO) running $$dir ...;               \
	    $(MAKE) -C $$dir run;                    \
	    $(ECHO);                                 \
	    $(ECHO);                                 \
	done

