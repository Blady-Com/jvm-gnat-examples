
API = /usr/local/gnat/lib/jre
# Points to the directory containing the Ada specs for the Java API

all : force
	@echo "Type \"make run\"  to run the example"

build : animate.jar

animate.jar :
	jvm-gnatmake -I${API} animate 
	jvm-gnatbind -n -I${API} animate.ali
	jvm-gnatlink animate.ali
	jarmake -o $@ animate\$$typ.class

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

