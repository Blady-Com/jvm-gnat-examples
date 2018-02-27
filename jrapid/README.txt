*******************************************************************
** RAPID
**
** By: Martin C. Carlisle
**     Jonathan Busch
**     W. Blair Watkinson II
**     United States Air Force Academy
**     Department of Computer Science
**
** See README-SOURCE.txt for copyright information.
**
** To get newest releases, visit:
** http://wuarchive.wustl.edu/languages/ada/usafa/rapid/index.html
**
** Submit bug reports/questions to:
** Martin Carlisle (carlislem@acm.org)
*******************************************************************

RAPID (the RAPID Ada Portable Interface Designer) is a GUI Design tool
written for and in Ada.  Currently, implementations exist for both
the JVM (using JGNAT), and Tcl/Tk.

See the docs subfolder for more information regarding RAPID.

The JVM version may be built by typing "make build".
To run RAPID on the JVM, type "make run".

See the makefile for command details.


BUGS
----

This is a beta release of JRAPID.  Submit additional bug reports (or even
better, fixes!) to Martin Carlisle (carlislem@acm.org).

Currently known bugs:

1) If you type a non-existent file in the File/Open dialog, the exception
   handler is not working.  Submitted to ACT.
2) Listboxes and dropdown boxes do not appear until another widget type
   has been selected.
3) A dropdown list can not be selected once it has been created.
4) On some RAPID dialogs, some labels are too long to fit in current font
   size.


DESCRIPTION OF FOLDERS
----------------------

classes    - Contains the jrapid image files (GIFs) and the pre-built jrapid
docs       - documentation see index.html
examples   - jrapid examples, see the README.txt inside that directory
jvm_peer   - JVM specific GUI sources for rapid
lib        - common libraries for all peers
mcc_gui    - The abstract GUI hierarchy as devised by Martin Carlisle
rapid      - sources for RAPID
