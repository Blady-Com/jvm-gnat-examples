JGNAT_JAR=$(dir $(shell which jvm-gnatls))../lib/jgnat.jar

API=/usr/local/gnat/lib/jre


all:
	@echo "Type \"make run\" to run the example"

mandelbrot.class : mandelbrot.adb gui.adb
	jvm-gnatmake -Pmandelbrot.gpr

wait.class : wait.java
	javac wait.java

build : mandelbrot.class wait.class

run: build
	@echo
	@echo WINDOWS users WARNING
	@echo
	@echo To properly see this application, once the window appears with
	@echo the COMPUTE button, and before clicking on the COMPUTE button,
	@echo enlarge the window to a full window by clicking on the enlarge
	@echo window button on the top right hand side of the window.
	@echo
	java -classpath $(JGNAT_JAR):. mandelbrot

clean:
	${RM} *.ali b~* *~ CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP) *.ads *.adb *.txt makefile *.java

force::




