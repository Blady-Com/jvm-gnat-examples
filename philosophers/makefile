
API = /usr/local/gnat/lib/jre
# Points to the directory containing the Ada specs for the Java API

all : force
	@echo "Type \"make run\" to run the example"

philosophers.jar :
	jvm-gnatmake -Pphilosophers.gpr
	jvm-gnat bind -Pphilosophers.gpr philosophers.ali
	jvm-gnat link -Pphilosophers.gpr philosophers.ali
	jarmake -o $@ philosophers\$$applet\$$typ.class

build : philosophers.jar

run : build
	appletviewer -J-Djava.security.policy=file.pol test.html

clean : force
	${RM}  *~ *.ali *.class CVS b~*

distclean : clean
	${RM} *.jar

# The following target is used only when building the windows release
flip :
	$(FLIP) makefile *.txt *.ads *.adb *.html

force:

