with Java.Lang.String;    use Java.Lang.String;
with Java.Awt.Color;
with Java.Awt.Dimension;  use Java.Awt.Dimension;
with Java.Awt.Image;      use Java.Awt.Image;
with Java.Applet.Applet;  use Java.Applet.Applet;
with Java.Awt.Graphics;   use Java.Awt.Graphics;
with Java.Net.URL;

package body Animate.Applet is

   ------------------------
   -- Local Types & Data --
   ------------------------

   Image_Size : Int := 65;

   type Overlap_Type is (Horiz, Vert, None);

   task Graphic_Task is
      entry Start (Draw_On : Ref; The_Image : Java.Awt.Image.Ref);
      entry Draw  (The_Task : Int; X, Y : in out Int);
   end Graphic_Task;

   ------------------
   -- Graphic_Task --
   ------------------

   task body Graphic_Task is
      App    : Ref;
      G      : access Java.Awt.Graphics.Typ'Class;
      Image  : access Java.Awt.Image.Typ'Class;
      Result : Boolean;

      type Position is record
         X, Y : Int := 0;
         Delta_X : Int := 2;
         Delta_Y : Int := 3;
         Should_Bounce : Overlap_Type := None;
      end record;

      type Pos_Array is array (Natural range <>) of Position;
      type Pos_Array_Ptr is access Pos_Array;

      Positions : Pos_Array (0 .. Num_Tasks - 1);

      -------------
      -- Overlap --
      -------------

      function Overlap (T1, T2 : Int) return Overlap_Type is
         function Square_Dist (X1, Y1, X2, Y2 : Int) return Int is
         begin
            return (X1 - X2) ** 2 + (Y1 - Y2) ** 2;
         end Square_Dist;

      begin
         if Square_Dist (Positions (T1).X, Positions (T1).Y,
                         Positions (T2).X, Positions (T2).Y) < Image_Size ** 2
         then
            if abs (Positions (T1).X - Positions (T2).X)
              < abs (Positions (T1).Y - Positions (T2).Y)
            then
               return Horiz;
            else
               return Vert;
            end if;
         else
            return None;
         end if;
      end Overlap;

   begin
      accept Start
        (Draw_On : Ref;
         The_Image : Java.Awt.Image.Ref)
      do
         App := Draw_On;
         G   := Getgraphics (App);
         Image := Java.Awt.Image.Ref(The_Image);

         for J in 1 .. Num_Tasks loop
            Positions (J - 1).Delta_Y := J;
         end loop;
      end Start;

      loop
         accept Draw (The_Task : Int; X, Y : in out Int) do

            --  The following statement is what is causing some part of the
            --  images to be deleted. This could be improved, for instance
            --  by drawing another pixmap that would be the mask, or using
            --  XOR mode for drawing
            ClearRect (G, Positions (The_Task).X, Positions (The_Task).Y,
                       GetWidth (Image, App.ImageObserver_I),
                       GetHeight (Image, App.ImageObserver_I));

            declare
               Delta_X : Int := Positions (The_Task).Delta_X;
               Delta_Y : Int := Positions (The_Task).Delta_Y;
            begin
               if Positions (The_Task).Should_Bounce = Horiz then
                  Delta_X := -Positions (The_Task).Delta_X;
               elsif Positions (The_Task).Should_Bounce = Vert then
                  Delta_Y := -Positions (The_Task).Delta_Y;
               else
                  Positions (The_Task).X := X + Positions (The_Task).Delta_X;
                  Positions (The_Task).Y := Y + Positions (The_Task).Delta_Y;

                  for J in 0 .. Num_Tasks - 1 loop
                     if J /= The_Task then
                        case Overlap (The_Task, J) is
                           when None => null;
                           when Horiz =>
                              Delta_X := -Positions (The_Task).Delta_X;
                              Positions (J).Should_Bounce := Horiz;
                           when Vert =>
                              Delta_Y := -Positions (The_Task).Delta_Y;
                              Positions (J).Should_Bounce := Vert;
                        end case;
                     end if;
                  end loop;
               end if;

               if X > Getsize (App).Width - Image_Size or else X < 0 then
                  Delta_X := -Positions (The_Task).Delta_X;
               end if;

               if Y > Getsize (App).Height - Image_Size or else Y < 0 then
                  Delta_Y := -Positions (The_Task).Delta_Y;
               end if;

               Positions (The_Task).Delta_X := Delta_X;
               Positions (The_Task).Delta_Y := Delta_Y;
            end;
            X := X + Positions (The_Task).Delta_X;
            Y := Y + Positions (The_Task).Delta_Y;
            Positions (The_Task).Should_Bounce := None;
            Positions (The_Task).X := X;
            Positions (The_Task).Y := Y;
            Result := DrawImage (G, Image, X, Y, App.ImageObserver_I);
         end Draw;
      end loop;
   end Graphic_Task;

   ----------
   -- Init --
   ----------

   procedure Init (This : access Typ)
   is
      procedure Adainit;
      pragma Import (Ada, Adainit, "ada_animate.adainit");
   begin
      Adainit;
   end Init;

   -----------
   -- Start --
   -----------

   procedure Start (This : access Typ) is
   begin
      Graphic_Task.Start
        (Ref (This), Java.Awt.Image.Ref(GetImage (This, GetCodeBase (This), +"./animate.gif")));

      for J in 0 .. Num_Tasks - 1 loop
         This.The_Animation := new Animation_Typ;
         This.The_Animation.Start (J, 20 + 50 * J, 30 + 50 * J);
      end loop;
   end Start;

   ------------
   -- Update --
   ------------

   procedure Update (This : access Typ;
                     G    : access java.awt.Graphics.Typ'Class) is
   begin
      --  try to get rid of the annoying "flashing" when the
      --  image is moved
      Paint (This, G);
   end Update;

   -------------------
   -- Animation_Typ --
   -------------------

   task body Animation_Typ is
      X, Y     : Int;
      My_Id    : Int;

   begin
      accept Start (Id : Int; Initial_X : Int; Initial_Y : Int) do
         X := Initial_X;
         Y := Initial_Y;
         My_Id := Id;
      end Start;

      loop
         Graphic_Task.Draw (My_Id, X, Y);
         delay Wait_Period;
      end loop;
   end Animation_Typ;

end Animate.Applet;
