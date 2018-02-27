------------------------------------------------------------------------------
--                                                                          --
--                             JGNAT DOCUMENTS                              --
--                                                                          --
--                   R E A D M E - E X A M P L E S . T X T                  --
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
--                                                                          --
-- JGNAT is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. JGNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with JGNAT; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        --
--         maintained by Ada Core Technologies, Inc & ACT Europe            --
--         http://www.gnat.com & http://www.act-europe.fr                   --
--                                                                          --
------------------------------------------------------------------------------


			    JGNAT EXAMPLES

				README


Contents
--------

1. Introduction
2. Java Development Kit to Use with the Examples
3. Examples Provided
4. How to Send us your Contributions

---------------------------------------------------------------------------


1. Introduction
   ------------

This directory contains a number of example programs to demonstrate the use of
the JGNAT system.  Each directory contains a README explaining the nature of
the example, source files for the program, and a makefile used to compile and
execute the program. All examples come precompiled. Just type "make run" in the
desired examples directory to run the example. If you want to run all examples
one after the other type "make run" in the directory where you found this file.
Some of the examples illustrate Ada programs that run as applications on the
JVM and others are gnapplets that must be run with appletviewer or a
Java-enabled browser.


2. Java Development Kit to Use with the Examples
   ---------------------------------------------

The examples assume that you are using Sun JDK 1.2.  If you would like to use
some other Java Development Kit, make sure it is (ideally) Java Platform 2
compliant and edit the makefiles to invoke the commands of your Java
Development Kit. All gnapplets run fine under netscape and Internet Explorer.


3. Examples Provided
   -----------------

Here is a short summary of the example programs included with this release:

   Simple text applications:
   ~~~~~~~~~~~~~~~~~~~~~~~~
   . acvc       - A small sample of Ada validation tests.
   . hello      - Trivial application that uses Text_IO.
   . prime      - An implementation of the sieve of Eratosthenes that
                  illustrates interfacing to a simple Java class to
                  perform text I/O.
   . quicksort  - A demonstration of the quicksort algorithm, illustrating
                  arrays, recursion, and nested subprograms.
   . text_io    - Simple application that demonstrates various Text_IO
                  features such as disk I/O, enumeration I/O, and user input.
   . waypoints  - A simple "waypoints" distance calculation program that makes
                  use of Generic_Elementary_Functions and Calendar contributed
                  by Rockwell Collins.

   The Java 1.2 API:
   ~~~~~~~~~~~~~~~~
   . java-API   - The complete set of Ada specs for the Java 2 Platform,
                  Standard Edition, v1.2.2 API Specification. This directory
                  contains a Makefile to compile all the specs.

   Graphical Applications:
   ~~~~~~~~~~~~~~~~~~~~~~
   . jrapid     - The Rapid GUI builder. This exciting application has been
                  contributed by Prof. Martin Carlisle of the US Force Academy
                  and illustrates the use of swing components from Ada. This is
                  also a very useful application in its own right to build
                  simple, but powerful GUI interfaces.
   . mandelbrot - A simple graphics application that calculates a Mandelbrot
                  fractal set and uses Java API classes to display the fractal
                  in a window.

   Gnapplets:
   ~~~~~~~~~
   . animate      - A small gnapplet (GNAT applet) that shows how GNAT can fly!
   . connect_four - A gnapplet implementing the game connect four contributed
                    by Professors Barry Fagin and Martin Carlisle of the US
                    Air Force Academy. Can you beat it.
   . philosophers - A dining philosophers simulation program contributed by 
                    Wiljan Derks (ITEC, Philips Semiconductors) that
                    illustrates Ada 95 tasking features.
   . tictactoe    - A gnapplet implementing a simple tic-tac-toe game.
                    Can you beat it?


4. How to Send us your Contributions
   ---------------------------------

We welcome contributions from JGNAT users.  If you develop a demonstration
application or gnapplet that you think is interesting or that showcases
JGNAT's features, please contact us at Ada Core Technologies or ACT Europe by
sending us e-mail to report@gnat.com if you'd like to submit it for possible
inclusion in later JGNAT releases.

The subject line for such submissions should be:

    Subject: JGNAT Contribution: <...subject to summarize contribution....>

The format of your submissions must be as detailed below. Please take
particular care of the requested targets in your makefile.


5. IMPORTANT Note on Format for Submissions
   ----------------------------------------

   (a) Please send each contribution in a separate message.  

   (b) Please include full sources along with a makefile to build the
       gnapplet or application and a detailed README explaining what the
       application or gnapplet does and how to operate it (a user's manual).

   (c) For applications that contain multiple separate compilation units, and
       hence multiple files, submit them at the end of your e-mail in the
       form of a single file that is acceptable input to gnatchop (i.e.,
       contains no non-Ada text).  If you use banners to separate the files,
       make sure they are composed entirely of blank lines or Ada
       comments. The first line of sources should be marked with an Ada
       comment line, and the sources should extend to the end of the
       message. If you must include a signature, make it look like an Ada
       comment.

   (d) Your makefile must have the following targets:
          - all       : must echo the following message: 
                        "Type  make run  to run the example".
          - build     : builds the application if not already built but it does
                        not rebuild it if all necessary files to run the
		        application are already present.
          - run       : depends on build and runs the application.
          - clean     : removes all files created during the build that are not
                        necessary to run the application.
          - distclean : remove all files created during the build process, 
                        putting the examples directory in its pristine state.
          - flip      : uses command $(FLIP) to flip text files from UNIX ASCII
                        to Windows (DOS) ASCII format. Should not flip binary
			files if present.
       See the makefiles in the examples already present for inspiration.

   (e) Please send all text in plain ASCII form. Make sure that your e-mail
       does not do something silly with your text such as breaking lines of a
       certain length .If your application uses image or sound files send
       them as attachments to your mail.

   (f) Thank you.



