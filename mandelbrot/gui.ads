with Java; use Java;
with Java.Awt.Canvas;
with Java.Awt.Color;
with Java.Awt.Event.ActionEvent;
with Java.Awt.Event.Actionlistener;
with Java.Awt.Event.WindowEvent;
with Java.Awt.Frame;
with Java.Awt.Graphics;
with Java.Awt.Image;
with Java.Awt.Event.WindowAdapter;
with Java.Lang.Object;
with Java.Lang.String;

package Gui is

   --  Create our own Canvas, since we want to define some new primitive
   --  operations on it (we want to save what is displayed in this canvas,
   --  so that refresh events are handled through a double-buffer pixmap).
   --
   --  All the types declared in this section have a convention Java, so that
   --  subprograms external names are correctly inherited.

   type My_Canvas is new Java.Awt.Canvas.Typ with
      record
         Graphics     : Java.Awt.Graphics.Ref;
         Img          : Java.Awt.Image.Ref;
         Img_Graphics : Java.Awt.Graphics.Ref;
      end record;
   type My_Canvas_Ref is access all My_Canvas'Class;
   pragma Convention (Java, My_Canvas);

   --  This represents our application. It inherits from Frame so as to create
   --  a separate window, and populates it with a button and a canvas.

   type My_Frame is new Java.Awt.Frame.Typ with
      record
         Canvas : My_Canvas_Ref;
      end record;
   type Ref is access all My_Frame'Class;
   pragma Convention (Java, My_Frame);

   --  The size of the window we create

   Width  : constant Int := 300;
   Height : constant Int := 300;

   ----------------------------------------
   --  Primitive operations for My_Frame --
   ----------------------------------------

   function New_My_Frame (Title : access Java.Lang.String.Typ'Class;
                          This : Ref := null) return Ref;
   pragma Java_Constructor (New_My_Frame);

   procedure Calculate (This : access My_Frame;
                        Width  : Int; Height : Int);

   -----------------------------------------
   --  Primitive operations for My_Canvas --
   -----------------------------------------

   function New_My_Canvas (This : My_Canvas_Ref := null) return My_Canvas_Ref;
   pragma Java_Constructor (New_My_Canvas);

   --  The operation called when the canvas needs to be refreshed. In our
   --  implementation of My_Canvas, this is just a matter of displaying
   --  the double-buffer pixmap

   procedure Paint (This : access My_Canvas;
                    G    : access Java.Awt.Graphics.Typ'Class);

   --  This function is called every time we want to display a pixel.
   --  It also updated the double buffer pixmap at the same time, so that the
   --  refresh can be accurate at any time.

   procedure Putpixel (This  : access My_Canvas;
                       X, Y  : Int;
                       Color : Java.Awt.Color.Ref);

   -----------------------------------------------------
   --  Primitive operations for My_Listener           --
   -----------------------------------------------------

   --  A listener is a special object that gets the events sent by the
   --  window. Every event is associated with a special primitive operation
   --  of that class, that is called when this event is raised. Note that
   --  the listener has to be registered by the window.

   type My_Listener is new Java.Awt.Event.WindowAdapter.Typ with null record;
   type My_Listener_Ref is access all My_Listener'Class;
   pragma Convention (Java, My_Listener);


   function New_Listener (This : My_Listener_Ref := null)
     return My_Listener_Ref;
   pragma Java_Constructor (New_Listener);

   --  This subprogram is called automatically every time the associated
   --  window is closing. This is used in our case to simply exit the
   --  application

   procedure windowClosing (This : access My_Listener;
                            P1 : access java.awt.event.WindowEvent.Typ'Class);

   -----------------------------------------------------
   --  Primitive operations for Start_Button_Listener --
   -----------------------------------------------------

   --  This listener follows the same principle as My_Listener, except it is
   --  associated with a button. The only action recognized here is
   --  actionPerformed, which will be called every time the button is pressed.

   type Start_Button_Listener
     (ActionListener_I : access Java.Awt.Event.ActionListener.Typ'Class)
   is new Java.Lang.Object.Typ with
      record
         Mandel : Ref;
      end record;
   type Start_Button_Listener_Ref is access all Start_Button_Listener'Class;
   pragma Convention (Java, Start_Button_Listener);

   function New_Start_Button_Listener
     (This : Start_Button_Listener_Ref := null)
     return Start_Button_Listener_Ref;
   pragma Java_Constructor (New_Start_Button_Listener);

   --  This function is called every time the button is pressed. In our case,
   --  it just starts the calculation

   procedure actionPerformed
     (This  : access Start_Button_Listener;
      Event : access Java.Awt.Event.ActionEvent.Typ'Class);
   pragma Convention (Java, actionPerformed);

   --------------------------
   --  General subprograms --
   --------------------------

   procedure Initialize;

end Gui;
