
all : force
	@echo "Type \"make run\" to run the example"

%.class : %.adb
	jvm-gnatmake $<

build : c32001c.class c32001d.class c53007a.class c54a03a.class c55b04a.class c56002a.class c58004g.class c64004g.class c64005b.class cb1001a.class cb2004a.class

run : build
	java c32001c
	java c32001d
	java c53007a
	java c54a03a
	java c55b04a
	java c56002a
	java c58004g
	java c64004g
	java c64005b
	java cb1001a
	java cb2004a

clean : force
	${RM}  *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP) *.ads *.adb makefile *.txt

force::

