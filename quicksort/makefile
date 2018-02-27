
all:
	@echo "Type \"make run\" to run the example"

qs.class :
	jvm-gnatmake qs

build: qs.class

run: build
	java qs

clean:
	${RM} *~ *.ali b~* CVS

distclean : clean
	${RM} *.class

# The following target is used only when building the windows release
flip :
	$(FLIP)  *.ads *.adb *.txt makefile

force:
