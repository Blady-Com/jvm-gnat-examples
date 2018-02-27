with Java.Awt.Borderlayout;
with Java.Awt.Button;
with Java.Awt.Image; use Java.Awt.Image;
with Java.Awt.Image.Imageobserver;
with Java.Awt.Layoutmanager;
with Java.Awt.Layoutmanager2; use Java.Awt.LayoutManager2;
with Java.Awt.Graphics; use Java.Awt.Graphics;
with Java.Awt.Event.WindowListener;
with Java.Lang.String; use Java.Lang.String;
with Java.Lang.System;

with Complex; use Complex;

package body Gui is

   Colors : array (0 .. 6) of Java.Awt.Color.Ref;

   -------------------
   -- New_My_Canvas --
   -------------------

   function New_My_Canvas (This : My_Canvas_Ref := null) return My_Canvas_Ref
   is
      --  The first, and only, thing a constructor has to do is to call
      --  the parent constructor. At this level, This has already been
      --  allocated by the compiler.
      Super : Java.Awt.Canvas.Ref := Java.Awt.Canvas.New_Canvas
        (Java.Awt.Canvas.Ref (This));
   begin
      return This;
   end New_My_Canvas;

   ------------------
   -- New_My_Frame --
   ------------------

   function New_My_Frame (Title : access Java.Lang.String.Typ'Class;
                          This  : Ref := null)
     return Ref
   is
      Super  : Java.Awt.Frame.Ref := Java.Awt.Frame.New_Frame
        (Title, Java.Awt.Frame.Ref (This));
      Button : Java.Awt.Button.Ref;
      Start  : Start_Button_Listener_Ref;
      Border : Java.Awt.BorderLayout.Ref
        := Java.Awt.BorderLayout.New_BorderLayout;
   begin

      --  A layout manager specifies how the widgets are organized within
      --  a container. There are many such layout managers available.

      Setlayout (This, Border.LayoutManager2_I.LayoutManager_I);

      Button := Java.Awt.Button.Ref(Java.Awt.Button.New_Button (+"Compute"));
      Add (This, Button, +"North");

      This.Canvas := New_My_Canvas;
      SetSize (This.Canvas, Width, Height);
      Add (This, This.Canvas, +"Center");

      --  Register a listener for the button. This listener is called every
      --  time an action needs to be performed after the user pressed the
      --  button

      Start := New_Start_Button_Listener;
      Java.Awt.Button.AddActionListener (Button, Start.ActionListener_I);
      Start.Mandel := Ref (This);

      --  Display the window on the screen

      --Show (This);  -- not in my API ???

      --  Creates some internal datas for the canvas. Note that the graphics
      --  should be created only after the window has been displayed with the
      --  call to Show above.

      This.Canvas.Img := Java.Awt.Image.Ref(CreateImage (This, Width, Height));
      This.Canvas.Graphics := Java.Awt.Graphics.Ref(Getgraphics (This.Canvas));
      This.Canvas.Img_Graphics := Java.Awt.Graphics.Ref(Getgraphics (This.Canvas.Img));

      --  Register the listener for the window. Every class registered at
      --  this level will be called every time some event happened on the
      --  window

      AddWindowListener (This, New_Listener.WindowListener_I);

      return This;
   end New_My_Frame;

   ------------------
   -- New_Listener --
   ------------------

   function New_Listener (This : My_Listener_Ref := null)
     return My_Listener_Ref
   is
      Super : Java.Awt.Event.WindowAdapter.Ref
        := Java.Awt.Event.WindowAdapter.New_WindowAdapter
        (Java.Awt.Event.WindowAdapter.Ref (This));
   begin
      return This;
   end New_Listener;

   -----------
   -- Paint --
   -----------

   procedure Paint (This : access My_Canvas;
                    G    : access Java.Awt.Graphics.Typ'Class) is
      B : Boolean;
   begin
      B := Java.Awt.Graphics.Drawimage
        (G, This.Img, 0, 0, Java.Awt.Color.Black,
         This.ImageObserver_I);
   end Paint;

   ---------------------
   -- ActionPerformed --
   ---------------------

   procedure ActionPerformed
     (This  : access Start_Button_Listener;
      Event : access java.awt.event.ActionEvent.Typ'Class)
   is
   begin
      Calculate (This.Mandel, Width, Height);
   end ActionPerformed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Gui : Ref;
   begin
      Colors (0) := Java.Awt.Color.Ref(Java.Awt.Color.Black);
      Colors (1) := Java.Awt.Color.Ref(Java.Awt.Color.Yellow);
      Colors (2) := Java.Awt.Color.Ref(Java.Awt.Color.Orange);
      Colors (3) := Java.Awt.Color.New_Color (255, 100, 0);
      Colors (4) := Java.Awt.Color.New_Color (160, 40, 0);
      Colors (5) := Java.Awt.Color.New_Color (150, 0, 0);
      Colors (6) := Java.Awt.Color.Ref(Java.Awt.Color.Black);
      Gui := New_My_Frame (+"GNAT Mandelbrot set");
      SetSize (Gui, Width, Height + 20);
      SetLocation (Gui, 100, 100);
   end Initialize;

   -------------------------------
   -- New_Start_Button_Listener --
   -------------------------------

   function New_Start_Button_Listener
     (This : Start_Button_Listener_Ref := null)
     return Start_Button_Listener_Ref
   is
      Super : Java.Lang.Object.Ref := Java.Lang.Object.New_Object
        (Java.Lang.Object.Ref (This));
   begin
      return This;
   end New_Start_Button_Listener;

   ---------------
   -- Calculate --
   ---------------

   procedure Calculate (This : access My_Frame;
                        Width  : Int; Height : Int)
   is
      Phyx1 : constant Float := -2.0;
      Phyy1 : constant Float := -2.0;
      Areax : constant Float := (1.0 - Phyx1);
      Areay : constant Float := (2.0 - Phyy1);
      Z     : Complex_Number;
      C     : Complex_Number;
      Step  : Natural;
      X, Y  : Int;
      H     : constant Int := Height / 2 + 1;
      Color : Java.Awt.Color.Ref;
   begin
      X := 0;
      while X < Width loop
         Y := 0;
         while Y < H loop
            Build (Float (X) * Areax / Float (Width)  + Phyx1,
                   Float (Y) * Areay / Float (Height) + Phyy1,
                   Z);

            Build (Real (Z), Imaginary (Z), C);
            Step := 0;

            while Norm (Z) < 4.0 and Step < 40 loop
               Square (Z, Z);
               Add (Z, C, Z);
               Step := Step + 1;
            end loop;

            if Norm (Z) < 4.0 then
               Color := Colors (0);
            elsif Step > 30 then
               Color := Colors (1);
            elsif Step > 20 then
               Color := Colors (2);
            elsif Step > 10 then
               Color := Colors (3);
            elsif Step > 5 then
               Color := Colors (4);
            elsif Step > 2 then
               Color := Colors (5);
            else
               Color := Colors (6);
            end if;

            PutPixel (This.Canvas, X, Y, Color);
            PutPixel (This.Canvas, X, Height - Y, Color);
            Y := Y + 1;
         end loop;

         X := X + 1;
      end loop;
   end Calculate;

   --------------
   -- Putpixel --
   --------------

   procedure Putpixel (This  : access My_Canvas;
                       X, Y  : Int;
                       Color : Java.Awt.Color.Ref)
   is
   begin
      --  Note: we can not call GetGraphics here, since it *creates*
      --  a new graphics every time it is called. It would be much too
      --  slow to call it for every pixel.
      Setcolor (This.Graphics, Color);
      DrawRect (This.Graphics, X, Y, 1, 1);

      SetColor (This.Img_Graphics, Color);
      DrawRect (This.Img_Graphics, X, Y, 1, 1);
   end Putpixel;

   ------------------
   -- windowClosed --
   ------------------

   procedure WindowClosing (This : access My_Listener;
                           P1 : access java.awt.event.WindowEvent.Typ'Class)
   is
   begin
      Java.Lang.System.Exit_K (0);
   end WindowClosing;

end Gui;
