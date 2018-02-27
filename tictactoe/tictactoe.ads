
with Java.Awt.Event.Mouselistener;
with Java.Awt.Event.MouseEvent;
with Java.Awt.Image;
with Java.Awt.Image.ImageObserver;
with Java.Awt.Graphics;
with Java.Applet.Applet;
with Java.Lang.String;
with Java.IO.Serializable;
with Java.Awt.MenuContainer;
with Javax.Accessibility.Accessible;

package TicTacToe is

   subtype Square is Integer range 0 .. 8;
   --  Index of a square in the game
   --  The game board has the following layout:
   --     0   1   2
   --     3   4   5
   --     6   7   8

   type Position is array (Square'Range) of Boolean;
   type Position_Access is access Position;
   --  Defines a position on the board (the bits are set when a square
   --  has a player in the corresponding square). To fully describe
   --  the game, two such arrays are needed, one for white and one
   --  for black.

   White : aliased Position;  --  white's current position (computer)
   Black : aliased Position;  --  black's current position (player)

   type Typ
     (I_Serializable  : java.io.Serializable.Ref;
      I_MenuContainer : java.awt.MenuContainer.Ref;
      I_ImageObserver : java.awt.image.ImageObserver.Ref;
      I_MouseListener : Java.Awt.Event.Mouselistener.Ref;
      I_Accessible    : Javax.Accessibility.Accessible.Ref)
   is new Java.Applet.Applet.Typ (I_MenuContainer,
                                  I_ImageObserver,
                                  I_Serializable,
                                  I_Accessible)
   with record
      First       : Boolean;            --  who goes first in the next game
      Is_New_Game : Boolean;            --  True if we are starting a new game
      NotImage    : Java.Awt.Image.Ref; --  Image for white
      CrossImage  : Java.Awt.Image.Ref; --  Image for black
   end record;
   pragma Convention (Java, Typ);
   --  Description of our Applet class.
   --  The discriminants indicate that this type "implements" (Java keyword)
   --  the interfaces ImageObserver and MouseListener. Every time one need
   --  to call a subprogram that expects one of these interfaces, we can
   --  simply supply the relevant discriminant.
   --
   --  Note also that this type "extends" (Java keyword) the Applet class,
   --  which itself was in the hierarchy of Java.Awt.Component that
   --  "implements" the interface ImageObserver. This is why we had to pass
   --  the discriminant to Applet.
   --
   --  Note also the use of pragma Convention (Java). This is so that some
   --  checks will be suppressed when generating the code, and that this
   --  type is fully compatible with the JVM.


   Moves : constant array (Position'Range) of Integer :=
     (4, 0, 2, 6, 8, 1, 3, 5, 7);
   --  Preferred order for squares: the computer should first try to play
   --  into square 4, then square 0, ...

   Done : constant Position := (others => True);
   --  This is the position of the game where every squared has been played in.

   type Game_Status is (OK, Win, Lose, Stalemate);
   --  Describes the state of a game

   -----------------------------------------
   --  Some general use functions
   -----------------------------------------

   --  These functions do not override any functions of the ancestor type.

   function Is_Won (Pos : access Position) return Boolean;
   --  Return True is the position described in Pos is a winning position

   function Best_Move (Board : access Typ) return Integer;
   --  Compute the best position for the computer (white).
   --  Returns the chosen square.

   function Your_Move (Board : access Typ; M : Square) return Boolean;
   --  User move. Returns True if the move is valid.

   procedure My_Move (Board : access Typ);
   -- Computer move. Chooses the best possible square to play in.

   function Status (Board : access Typ) return Game_Status;
   --  Returns the status of the game (Win, Lose, Stalemate).

   -----------------------------------------
   --  Overrides some functions from the parent class Applet
   -----------------------------------------

   function GetAppletInfo (This : access Typ) return Java.Lang.String.Ref;
   --  Returns a string to be displayed when the user presses the "info"
   --  button in the Appletviewer or Netscape
   --
   --  Note: This function overrides a function in the parent class Applet.
   --  The external name for the JVM is automatically chosen by JGNAT to be
   --  the same as the function that is overridden ("getAppletInfo"), so
   --  there is no need to give a pragma Export (Java, ...)

   procedure Init (This : access Typ);
   --  This function is automatically called by the JVM once the applet
   --  has been loaded and allocated. This is the main entry point for the
   --  applet, where arbitrary initialization can be performed.

   procedure Paint (This : access Typ;
                    G1   : access Java.Awt.Graphics.Typ'Class);
   --  This function is called every time the applet needs to be repainted.
   --  We can draw lines, put pixmaps, etc.

   procedure Update (This : access Typ;
                     G    : access java.awt.Graphics.Typ'Class);
   --  This is called every time the image needs to be repainted. The default
   --  behavior in Java.Awt.Component is to erase the widget and then call
   --  Paint.
   --  To avoid flashing, we do not want to erase it in our case.

   -----------------------------------------
   --  Redefines the functions found in the interface MouseListener
   -----------------------------------------

   --  Note: these functions override the functions declared in the Java
   --  interface mouseListener. The external names for the JVM are
   --  automatically inherited from java-awt-event-mouselistener.ads

   procedure mouseReleased (This : access Typ;
                            E : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseReleased);

   procedure mouseClicked (This : access Typ;
                           P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseClicked);

   procedure mouseEntered (This : access Typ;
                           P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseEntered);

   procedure mouseExited (This : access Typ;
                          P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseExited);

   procedure mousePressed (This : access Typ;
                           P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MousePressed);

end TicTacToe;

