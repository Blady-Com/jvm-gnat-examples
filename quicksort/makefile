JGNAT_JAR=$(dir $(shell which jvm-gnatls))../lib/jgnat.jar

all:
	@echo "Type \"make run\" to run the example"

qs.class :
	jvm-gnatmake qs

build: qs.class

run: build
	java -classpath $(JGNAT_JAR):. qs

clean:
	${RM} *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP)  *.ads *.adb *.txt makefile

force:
