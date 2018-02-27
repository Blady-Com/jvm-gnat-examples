with Java;                         use Java;
with Java.Awt.Image;
with Java.Applet.Applet;
with Java.Awt.Image.ImageObserver;
with Java.Awt.Graphics;

package Animate is

   Num_Tasks : constant := 5;
   --  The overall number of GNAT bugs that will fly on the screen

   Wait_Period : constant := 0.01;
   --  Increasing this value will slow down the animation, decreasing
   --  it will speed it up.

   task type Animation_Typ is
      entry Start (Id : Int; Initial_X : Int; Initial_Y : Int);
   end Animation_Typ;
   type Animation is access Animation_Typ;

   --  Type Typ implements the same interfaces as Applet.Typ and no
   --  new interfaces, so we do not need to add discriminants to the
   --  definition of Typ below.
   --  If you do not understand this comment read section
   --  "Using Java Interfaces" of the JGNAT User's Guide.

   type Typ is new Java.Applet.Applet.Typ with record
      The_Animation : Animation;
   end record;
   type Ref is access all Typ'Class;
   pragma Convention (Java, Typ);

   procedure Init   (This : access Typ);
   procedure Start  (This : access Typ);
   procedure Update (This : access Typ;
                     G    : access java.awt.Graphics.Typ'Class);

end Animate;
