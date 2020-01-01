# JVM-GNAT Examples

JVM-GNAT is the Ada development environment for the Java Virtual Machine. From Ada source code, the compiler generate ByteCode class files compatible with Java Runtime Environment 1.5.

All JVM-GNAT examples are updated for JVM-GNAT GPL 2013, though some compiler issues may appear with JRapid and Mandelbrot programs and all applet executions issue an error "java.lang.NullPointerException", see status below.

| Program      |JVM-GNAT GPL 2013| Examples status |
|:------------ |:------------:| ---------------:|
| acvc         | ok           |                 |
| animate      | exec error   | java.lang.NullPointerException |
| connect_four | exec error   | java.lang.NullPointerException |
| hello        | ok           |                 |
| jrapid       | comp error   | Program_Error exp_util.adb:2407 explicit raise, Error detected at mcc-common_dialogs.adb:283:49|
| mandelbrot   | comp error   | Assert_Failure jvm.adb:4217, Error detected at gui.adb:30:7 |
| philosophers | exec error   | gnat.adalib.program_error: forks.ads:13 finalize/adjust raised exception |
| prime        | ok           |                 |
| quicksort    | ok           |                 |
| text_io      | ok           |                 |
| tictactoe    | exec error   | java.lang.NullPointerException |
| waypoints    | ok           |                 |

See original [readme](https://github.com/Blady-Com/jvm-gnat-examples/blob/master/README-EXAMPLES.txt).

Pascal Pignard, March 2018.
