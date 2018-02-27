
all:
	@echo "Type \"make run\" to run the example"

hello.class :
	jvm-gnatmake hello

build : hello.class

run: build
	java hello

clean:
	${RM} *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP) *.adb *.txt makefile

force:
