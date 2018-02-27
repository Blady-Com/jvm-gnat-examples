--  This is a java applet which shows an animation of the dining
--  philosophers. When there is food on the plate, they want to eat.
--  Written by Wiljan Derks.

with java;                         use java;
with java.Applet.Applet;
with java.Awt.Graphics;
with Forks; use Forks;

package Philosophers is

   Max_Philosophers : constant := 5;

   type Typ;
   type Ref is access all Typ'Class;

   type Info;
   type Info_Access is access Info;

   task type Philosopher_Task (App : Ref; I : Info_Access);
   type Philosopher_Task_Access is access Philosopher_Task;
   type State_Type is
      (Idle,    --  Wants nothing
       Hunger,  --  Has hunger but no forks to eat
       Left,    --  Has hunger and has left fork
       Right,   --  Has hunger and has right fork to eat
       Eating); --  Has both forks and is eating

   type Info is
      record
         Next  : Info_Access;
         Fork  : Fork_Type;
         Phil  : Philosopher_Task_Access;
         State : State_Type := Idle;
      end record;

   type Typ is new java.applet.Applet.Typ with
      record
         Data : Info_Access;
      end record;
   pragma Convention (Java, Typ);

   procedure Init (This : access Typ);
   procedure Start (This : access Typ);
   procedure Paint (This : access Typ;
                    G : access java.awt.Graphics.Typ'Class);

end Philosophers;
