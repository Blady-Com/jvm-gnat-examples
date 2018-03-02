with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Discrete_Random;
with java.awt.Rectangle;                use java.awt.Rectangle;
with java.awt.Graphics;                 use java.awt.Graphics;
with java.awt.Color;                    use java.awt.Color;

package body Philosophers.Applet is

   function Random return Integer;
   function Total (This : access Typ) return Integer;
   procedure Polar_To_XY
     (X_Offset, Y_Offset : Integer;
      Angle, Length : Integer;
      X, Y : out Integer);

   package My_Random is new Ada.Numerics.Discrete_Random (Integer);
   use My_Random;

   G : Generator;

   ------------
   -- Random --
   ------------

   function Random return Integer is
   begin
      return Random (G);
   end Random;

   ----------------------
   -- Philosopher_Task --
   ----------------------

   task body Philosopher_Task is
   begin
      loop
         I.State := Hunger;
         repaint (App);

         Grap_Fork (I.Fork);

         I.State := Left;
         repaint (App);

         Grap_Fork (I.Next.Fork);

         I.State := Eating;
         repaint (App);

         delay Duration (Random mod 4000) / 1000.0;

         Release_Fork (I.Fork);
         Release_Fork (I.Next.Fork);

         I.State := Idle;
         repaint (App);

         delay Duration (Random mod 4000) / 1000.0;
      end loop;
   end Philosopher_Task;

   ----------
   -- Init --
   ----------

   procedure Init (This : access Typ) is
      procedure Adainit;
      pragma Import (Ada, Adainit, "ada_philosophers.adainit");
   begin
      Adainit;
   end Init;

   -----------
   -- Start --
   -----------

   procedure Start (This : access Typ) is
      P, Last : Info_Access;
   begin
      Last := new Info;
      Last.Fork := Create_Fork;
      This.Data := Last;
      for I in 2 .. Max_Philosophers loop
         P := new Info;
         P.Fork := Create_Fork;
         P.Next := This.Data;
         This.Data := P;
      end loop;
      Last.Next := This.Data;
      P := This.Data;
      loop
         P.Phil := new Philosopher_Task (Ref (This), P);
         P := P.Next;
         exit when P = This.Data;
      end loop;
   end Start;

   -----------
   -- Total --
   -----------

   function Total (This : access Typ) return Integer is
      P : Info_Access := This.Data;
      Count : Integer := 0;
   begin
      loop
         P := P.Next;
         Count := Count + 1;
         exit when P = This.Data;
      end loop;
      return Count;
   end Total;

   -----------------
   -- Polar_To_XY --
   -----------------

   procedure Polar_To_XY
     (X_Offset, Y_Offset : Integer;
      Angle, Length : Integer;
      X, Y : out Integer)
   is
   begin
      X := X_Offset +
           Integer (Float (Length) * Sin (2.0 * Pi / 360.0 * Float (Angle)));
      Y := Y_Offset +
           Integer (Float (Length) * Cos (2.0 * Pi / 360.0 * Float (Angle)));
   end Polar_To_XY;

   -----------
   -- Paint --
   -----------

   procedure Paint
     (This : access Typ;
      G : access java.awt.Graphics.Typ'Class)
   is
      Tot        : constant Integer := Total (This);
      Angle_Step : constant Integer := 360 / Tot;
      Max        : constant Integer :=
         Integer'Min (getBounds (This).height,
                      getBounds (This).width) - 1;
      Plate_Size : constant Integer := Max / 5;

      P : Info_Access;
      Angle : Integer := 0;
      X, Y, X1, Y1, X2, Y2 : Integer;
   begin
      setColor (G, lightGray);
      fillArc (G, 0, 0, Max, Max, 0, 360);
      setColor (G, black);
      drawArc (G, 0, 0, Max, Max, 0, 360);
      P := This.Data;
      loop
         Polar_To_XY (Max / 2, Max / 2,
                      Angle + Angle_Step / 2,
                      Max / 2 - Plate_Size * 3 / 4,
                      X, Y);
         setColor (G, blue);
         fillArc (G,
                  X - Plate_Size / 2, Y - Plate_Size / 2,
                  Plate_Size, Plate_Size,
                  0, 360);
         setColor (G, black);
         drawArc (G,
                  X - Plate_Size / 2, Y - Plate_Size / 2,
                  Plate_Size, Plate_Size,
                  0, 360);
         if P.State /= Idle then
            setColor (G, yellow);
            X2 := X; Y2 := Y;
            for I in 1 .. 50 loop
               Polar_To_XY (X, Y, Random mod 360, Random mod (Plate_Size / 2),
                            X1, Y1);
               drawLine (G, X1, Y1, X2, Y2);
               X2 := X1;
               Y2 := Y1;
            end loop;
         end if;
         if P.State = Left or P.State = Eating then
            Polar_To_XY (Max / 2, Max / 2,
                         Angle + Angle_Step / 6, Max / 2,
                         X1, Y1);
            setColor (G, black);
            drawLine (G, X1, Y1, X, Y);
         end if;
         if P.State = Right or P.State = Eating then
            Polar_To_XY (Max / 2, Max / 2,
                         Angle + Angle_Step * 5 / 6, Max / 2,
                         X1, Y1);
            setColor (G, black);
            drawLine (G, X1, Y1, X, Y);
         end if;
         if P.State /= Right and then
            P.State /= Eating and then
            P.Next.State /= Left and then
            P.Next.State /= Eating
         then
            Polar_To_XY (Max / 2, Max / 2,
                         Angle + Angle_Step, Max / 2,
                         X1, Y1);
            Polar_To_XY (Max / 2, Max / 2,
                         Angle + Angle_Step, Max / 4,
                         X2, Y2);
            setColor (G, black);
            drawLine (G, X1, Y1, X2, Y2);
         end if;
         Angle := Angle + Angle_Step;
         P := P.Next;
         exit when P = This.Data;
      end loop;
   end Paint;

end Philosophers.Applet;
