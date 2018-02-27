all:
	@echo "Type \"make run\" to run the example"

demo.class :
	jvm-gnatmake demo.adb

build : demo.class

run : build
	java demo

clean:
	${RM} *.ali CVS toto.txt b~* *~

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP)  *.adb *.txt makefile

force:
