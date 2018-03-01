JGNAT_JAR=$(dir $(shell which jvm-gnatls))../lib/jgnat.jar

all:
	@echo type \"make run\" to run the example

main.class :
	jvm-gnatmake  main

build : main.class

run : build
	java -classpath $(JGNAT_JAR):. main

clean :
	${RM} *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP)  *.ads *.adb *.txt makefile

force :

