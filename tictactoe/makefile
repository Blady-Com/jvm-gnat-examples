
API = /usr/local/gnat/lib/jre
# Points to the directory containing the Ada specs for the Java API

all : force
	@echo "Type \"make run\" to run the example"

tictactoe.jar :
	jvm-gnatmake -I${API} tictactoe
	jvm-gnatbind -n -I${API} tictactoe.ali
	jvm-gnatlink tictactoe.ali
	jarmake -o $@ tictactoe\$$typ.class

build : tictactoe.jar

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

