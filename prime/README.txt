This is a demonstration of JGNAT using a simple prime number generator.  The
test consists of a main subprogram Prime (in prime.adb), which uses package
Simple_IO whose body is written in Java. This test could just as easily have
been written to use Ada.Text_IO, but here we show a simple example of how to
interface to Java via an imported package that declares Java-imported
subprograms.  This test also demonstrates the use of an exception handler to
handler invalid input.

To execute the demo, simply type 'make run'.  Enter the number of prime
numbers to generate when prompted by the program.
