This directory contains a demonstration program for a small gnapplet (GNAT
applet), implemented in Ada and compiled with JGNAT, that illustrates the use
of tasking (each Ada task is mapped to a Java thread by JGNAT).  The program
draws several "GNAT" bugs that fly around within a window, each bug being
modeled by a task, and proves that JGNAT really does fly!

The number of flying GNATs as well as the speed of the overall animation are
controlled by two constants in file animate.ads.
