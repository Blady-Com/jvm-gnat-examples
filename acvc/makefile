JGNAT_JAR=$(dir $(shell which jvm-gnatls))../lib/jgnat.jar

all : force
	@echo "Type \"make run\" to run the example"

%.class : %.adb
	jvm-gnatmake $<

build : c32001c.class c32001d.class c53007a.class c54a03a.class c55b04a.class c56002a.class c58004g.class c64004g.class c64005b.class cb1001a.class cb2004a.class

run : build
	java -classpath $(JGNAT_JAR):. c32001c
	java -classpath $(JGNAT_JAR):. c32001d
	java -classpath $(JGNAT_JAR):. c53007a
	java -classpath $(JGNAT_JAR):. c54a03a
	java -classpath $(JGNAT_JAR):. c55b04a
	java -classpath $(JGNAT_JAR):. c56002a
	java -classpath $(JGNAT_JAR):. c58004g
	java -classpath $(JGNAT_JAR):. c64004g
	java -classpath $(JGNAT_JAR):. c64005b
	java -classpath $(JGNAT_JAR):. cb1001a
	java -classpath $(JGNAT_JAR):. cb2004a

clean : force
	${RM}  *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP) *.ads *.adb makefile *.txt

force::

