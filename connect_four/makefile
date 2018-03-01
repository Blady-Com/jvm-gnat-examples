
API = /usr/local/gnat/lib/jre
# Points to the directory containing the Ada specs for the Java API

all : force
	@echo "Type \"make run\" to run the example"

build : connectfour.jar

connectfour.jar :
	jvm-gnatmake -Pconnectfour
	jvm-gnat bind -Pconnectfour connectfour.ali
	jvm-gnat link -Pconnectfour connectfour.ali
	jarmake -o $@ connectfour\$$applet\$$typ.class

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

