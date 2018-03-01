
API = /usr/local/gnat/lib/jre
# Points to the directory containing the Ada specs for the Java API

all : force
	@echo "Type \"make run\" to run jrapid"

classes/jrapid.jar :
	jvm-gnatmake -Pjrapid.gpr
	( cd classes ;                                                              \
	  jarmake -o jrapid.jar META-INF/MANIFEST.MF rapid.class *.gif *.gui )

build : classes/jrapid.jar

run : build
	cd classes ; java -jar jrapid.jar

clean :
	cd .                 ; $(RM) CVS
	cd classes           ; $(RM) *.ali *.class b~* CVS META-INF/CVS
	cd docs              ; $(RM) CVS
	cd examples          ; $(RM) CVS
	cd jvm_peer          ; $(RM) CVS
	cd lib               ; $(RM) CVS
	cd mcc_gui           ; $(RM) CVS
	cd rapid             ; $(RM) CVS

distclean : clean
	${RM} classes/*.jar

# The following target is used only when building the windows release
flip :
	cd .                 ; $(FLIP) makefile *.txt
	cd examples          ; $(FLIP) *.txt *.ads *.adb
	cd jvm_peer          ; $(FLIP) *.ads *.adb
	cd lib               ; $(FLIP) *.ads *.adb
	cd mcc_gui           ; $(FLIP) *.ads
	cd rapid             ; $(FLIP) *.ads *.adb

force:
