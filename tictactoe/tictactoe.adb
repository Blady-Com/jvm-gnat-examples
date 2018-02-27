
--  The algorithm used in this program is really trivial, and will not
--  be detailed (it just checks every possible square and chooses the one
--  results in a win; if there is none, it simply picks the first found).
--
--  This demonstrates some technical aspects of programming with JGNAT.
--  You can see that you don't have to change any of your coding habits,
--  as JGNAT can handle any Ada 95 code.


with Java.Lang.String;    use Java.Lang.String;
with Java;                use Java;
with Java.Applet.Applet;  use Java.Applet.Applet;
with Java.Awt.Color;
with Java.Awt.Dimension;
with Java.Awt.Graphics;   use Java.Awt.Graphics;
with Java.Net.URL;

package body TicTacToe is

   ------------
   -- Is_Won --
   ------------

   function Is_Won (Pos : access Position) return Boolean is
   begin
      if Pos (0) then
         if (Pos (1) and then Pos (2))
           or else (Pos (3) and then Pos (6))
           or else (Pos (4) and then Pos (8))
         then
            return True;
         end if;

      elsif Pos (1) and then Pos (4) and then Pos (7) then
         return True;

      elsif Pos (2) then
         if (Pos (5) and then Pos (8))
           or else (Pos (4) and then Pos (6))
         then
            return True;
         end if;

      elsif Pos (3) and then Pos (4) and then Pos (5) then
         return True;

      elsif Pos (6) and then Pos (7) and then Pos (8) then
         return True;
      end if;
      return False;
   end Is_Won;

   ---------------
   -- Best_Move --
   ---------------

   function Best_Move (Board : access Typ)
     return Integer is

      Best_Move : Integer := -1;
      Mw        : Integer;
   begin

      --  Print_Array (White'Access);
      --  Print_Array (Black'Access);
      for I in Position'Range loop
         Mw := Moves (I);

         --  If nobody has played on that square before
         if not White (Mw) and then not Black (Mw) then

            --  If we can't find a move that gives an automatic winning
            --  position, then this one will be the one we want to play
            if Best_Move = -1 then
               Best_Move := Mw;
            end if;

            White (Mw) := True;

            --  If white wins, take it
            if Is_Won (White'Access) then
               White (Mw) := False;
               return Mw;
            end if;

            for Mb in Position'Range loop
               if not White (Mb) and then not Black (Mb) then
                  Black (Mb) := True;

                  --  If black would win, take another
                  if Is_Won (Black'Access) then
                     Black (Mb) := False;
                     White (Mw) := False;

                     --  If this was the default position we would have played
                     --  on, just find another one.
                     if Best_Move = Mw then
                        Best_Move := -1;
                     end if;
                     exit ;
                  end if;
                  Black (Mb) := False;
               end if;
            end loop;

            White (Mw) := False;
         end if;
      end loop;


      if Best_Move /= -1 then
         return Best_Move;
      end if;

      --  Take the first move that is open
      for I in Position'Range loop
         Mw := Moves (I);
         if not White (Mw) and then not Black (Mw) then
            return Mw;
         end if;
      end loop;

      --  no more moves
      return -1;
   end Best_Move;


   ---------------
   -- Your_Move --
   ---------------

   function Your_Move (Board : access Typ; M : Square) return Boolean is
   begin
      --  If the square is occupied
      if Black (M) or White (M) then
         return False;
      end if;
      Black (M) := True;
      return True;
   end Your_Move;

   -------------
   -- My_Move --
   -------------

   procedure My_Move (Board : access Typ) is
      Best : Integer;
   begin
      Best := Best_Move (Board);
      if Best = -1 then
         return;
      end if;
      White (Best) := True;
   end My_Move;

   ------------
   -- Status --
   ------------

   function Status (Board : access Typ) return Game_Status is
   begin
      if Is_Won (White'Access) then
         return Win;
      elsif Is_Won (Black'Access) then
         return Lose;
      elsif (Black or White) = Done then
         --  Note above that the array operator or is correctly handled
         --  by Jgnat.
         return Stalemate;
      else
         return Ok;
      end if;
   end Status;

   ----------
   -- Init --
   ----------

   procedure Init (This : access Typ) is
      procedure Adainit;
      pragma Import (Ada, Adainit, "ada_tictactoe.adainit");
   begin
      Adainit;
      --  The above call is needed for elaboration

      This.NotImage   :=
        Java.Awt.Image.Ref(GetImage (This, GetCodeBase (This), +"./not.gif"));
      This.CrossImage :=
        Java.Awt.Image.Ref(GetImage (This, GetCodeBase (This), +"./cross.gif"));

      AddMouseListener (This, This.I_MouseListener);
      This.Is_New_Game := True;
      This.First := True;
   end Init;

   -----------
   -- Paint --
   -----------

   procedure Paint (This : access Typ;
                    G1   : access Java.Awt.Graphics.Typ'Class)
   is
      D    : Java.Awt.Dimension.Ref := Java.Awt.Dimension.Ref(GetSize (This));
      Xoff : int := D.Width / 3;
      Yoff : int := D.Height / 3;
      I    : Integer;
      Tmp  : Boolean;
   begin
      if This.Is_New_Game then
         ClearRect (G1, 0, 0, D.Width, D.Height);
         This.Is_New_Game := False;
      end if;

      SetColor (G1, Java.Awt.Color.Red);
      DrawLine (G1, Xoff, 0, Xoff, D.Height);
      DrawLine (G1, 2 * Xoff, 0, 2 * Xoff, D.Height);
      DrawLine (G1, 0, Yoff, D.Width, Yoff);
      DrawLine (G1, 0, 2 * Yoff, D.Width, 2 * Yoff);
      I := 0;

      for R in 0 .. 2 loop
         for C in 0 .. 2 loop
            if White (I) then
               Tmp := DrawImage (G1, This.NotImage,
                                 C * Xoff + 1, R * Yoff + 1,
                                 Xoff, Yoff, This.I_ImageObserver);
            elsif Black (I) then
               Tmp := DrawImage (G1, This.CrossImage,
                                 C * Xoff + 1, R * Yoff + 1,
                                 Xoff, Yoff, This.I_ImageObserver);
            end if;
            I := I + 1;
         end loop;
     end loop;
   end Paint;

   ------------
   -- Update --
   ------------

   procedure Update (This : access Typ; G : access java.awt.Graphics.Typ'Class)
   is
   begin
      Paint (This, G);
   end Update;

   -------------------
   -- GetAppletInfo --
   -------------------

   function GetAppletInfo (This : access Typ) return Java.Lang.String.Ref is
   begin
      return +("This TicTacToe was coded in Ada95, "
               & "and compiled with Jgnat compiler");
   end GetAppletInfo;

   -------------------
   -- mouseReleased --
   -------------------

   procedure mouseReleased (This : access Typ;
                            E    : access java.awt.event.MouseEvent.Typ'Class)
   is
      X : Int := Java.Awt.Event.MouseEvent.GetX (E);
      Y : Int := Java.Awt.Event.MouseEvent.GetY (E);
      D : Java.Awt.Dimension.Ref := Java.Awt.Dimension.Ref(GetSize (This));

      --  Figure out the row/column
      C : Int := (X * 3) / D.Width;
      R : Int := (Y * 3) / D.Height;
   begin
      case Status (This) is
         when Win | Lose | Stalemate =>
            White := (others => False);
            Black := (others => False);
            if This.First then
               White (4) := True;  --  should start at a random place
            end if;
            This.First := not This.First;
            This.Is_New_Game := True;
            Repaint (This);
            return;
         when others =>
            null;
      end case;

      if Your_Move (This, Square (C + R * 3)) then
         My_Move (This);
         Repaint (This);
      end if;
   end MouseReleased;


   --  The functions below do nothing, but are required to override the ones
   --  defined in the interface we are implementing (when they abstract).
   --  Otherwise, the JVM would complain.

   ------------------
   -- mouseClicked --
   ------------------

   procedure mouseClicked (This : access Typ;
                           P1   : access java.awt.event.MouseEvent.Typ'Class)
   is
   begin
      null;
   end MouseClicked;

   ------------------
   -- mouseEntered --
   ------------------

   procedure mouseEntered (This : access Typ;
                           P1   : access java.awt.event.MouseEvent.Typ'Class)
   is
   begin
      null;
   end MouseEntered;

   -----------------
   -- mouseExited --
   -----------------

   procedure mouseExited (This : access Typ;
                          P1   : access java.awt.event.MouseEvent.Typ'Class) is
   begin
      null;
   end MouseExited;

   ------------------
   -- mousePressed --
   ------------------

   procedure mousePressed (This : access Typ;
                           P1   : access java.awt.event.MouseEvent.Typ'Class)
   is
   begin
      null;
   end MousePressed;

end TicTacToe;
