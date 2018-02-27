
API = /usr/local/gnat/lib/jre
# Points to the directory containing the Ada specs for the Java API

all : force
	@echo "Type \"make run\" to run the example"

build : connectfour.jar

connectfour.jar :
	jvm-gnatmake -I${API} connectfour
	jvm-gnatbind -n -I${API} connectfour.ali
	jvm-gnatlink connectfour.ali
	jarmake -o $@ connectfour\$$typ.class

run : build
	appletviewer -J-Djava.security.policy=file.pol test.html

clean : force
	${RM}  *~ *.ali *.class b~* CVS

distclean : clean
	${RM} *.jar

# The following target is used only when building the windows release
flip :
	$(FLIP) makefile *.txt *.ads *.adb *.html

force:

