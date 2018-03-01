JGNAT_JAR=$(dir $(shell which jvm-gnatls))../lib/jgnat.jar

all:
	@echo "Type \"make run\" to run the example"


simple_io.class : simple_io.java
	javac simple_io.java

prime.class: prime.adb
	jvm-gnatmake prime.adb

build : prime.class simple_io.class

run: build
	java -classpath $(JGNAT_JAR):. prime

clean:
	${RM} *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP) *.ads *.adb *.txt makefile *.java

force:
