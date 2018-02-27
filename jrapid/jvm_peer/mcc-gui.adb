-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC GUI PACKAGE LIBRARY                               
--           Copyright (C) 1999 Martin C. Carlisle.                
--                                                                 
-- RAPID is free software;  you can  redistribute it  and/or modify
-- it under terms of the  GNU General Public License as published  
-- by the Free Software  Foundation;  either version 2,  or (at your
-- option) any later version.  RAPID is distributed in the hope that 
-- it will be useful, but WITHOUT ANY WARRANTY;  without even the 
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU General Public License for more details.  
-- You should have  received  a copy of the GNU General Public License
-- distributed with RAPID; see file COPYING.  If not, write to the 
-- Free Software Foundation,  59 Temple Place - Suite 330,  Boston, 
-- MA 02111-1307, USA.                                                 
--                                                                     
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an 
-- executable, this unit does not by itself cause the resulting 
-- executable to be covered by the GNU General Public License.  
-- This exception does not however invalidate any other reasons 
-- why the executable file might be covered by the GNU Public 
-- License.  This exception does not apply to executables which
-- are GUI design tools, or that could act as a replacement
-- for RAPID.
------------------------------------------------------------------------------
with Java.Awt.Component;
with Java.awt.Container;
with Java.Awt.Color;
with Java.Awt.Point;
with Java.Awt.Frame;
with Java.Awt.Toolkit;
with Javax.Swing.JComponent;
with Javax.Swing.JFrame;
with Java.Awt.Cursor;
with Java.Awt.Window;
with Java.Awt.Container;
with Peer.Listeners;
with Peer.Scrollpane;
with Ada.Unchecked_Conversion;
with Java.Awt.Event.KeyListener;
with Java.Awt.Event.MouseListener;
with Java.Awt.Event.MouseMotionListener;

package body Mcc.Gui is

   ----------
   -- Bell --
   ----------

   procedure Bell is
   begin
      Java.Awt.Toolkit.Beep(Java.Awt.Toolkit.getDefaultToolkit);
   end Bell;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Obj : in Object) is
      Parent : Java.Awt.Container.Ref;
   begin
      -- if a frame, then dispose, o/w remove from parent.
      if Obj.My_Peer.all in Javax.Swing.JFrame.Typ'Class then
         Java.Awt.Window.Dispose(Java.Awt.Window.Ref(Obj.My_Peer));
      else
         Parent := Java.Awt.Container.Ref(Java.Awt.Component.getParent(
               Java.Awt.Component.Ref(Obj.My_Peer)));
            
         Java.Awt.Container.Remove(
            Parent,
            Java.Awt.Component.Ref(Obj.My_Peer));
            
         Java.Awt.Container.Repaint(Parent);
      end if;
   end Destroy;

   -----------------------------------------
   -- Since I can't remove a Java KeyListener
   -- once I've registered it (I don't keep a 
   -- pointer anywhere), I instead have it
   -- run these dummy procedures.  In my Get
   -- selectors, I catch a pointer to these
   -- and return null instead
   --
   -- Note that this means I must be careful
   -- to reset the pointer to null on every
   -- create
   -----------------------------------------
   procedure Dummy_Key_Listener(
      Obj   : in out Sized_Object'Class;
      Event : in     Key_Event) is
   begin
      null;
   end Dummy_Key_Listener;

   procedure Dummy_Mouse_Listener(
      Obj   : in out Sized_Object'Class;
      Event : in     Mouse_Event) is
   begin
      null;
   end Dummy_Mouse_Listener;

   ----------------------
   -- Get_Key_Listener --
   ----------------------

   function Get_Key_Listener
     (Obj : in Sized_Object'Class)
      return Key_Listener
   is
   begin
      if Obj.Key_Handler /= Dummy_Key_Listener'Access then
         return Obj.Key_Handler;
      else
         return null;
      end if;
   end Get_Key_Listener;

   ------------------------
   -- Get_Mouse_Listener --
   ------------------------

   function Get_Mouse_Listener
     (Obj : in Sized_Object'Class)
      return Mouse_Listener
   is
   begin
      if Obj.Mouse_Handler /= Dummy_Mouse_Listener'Access then
         return Obj.Mouse_Handler;
      else
         return null;
      end if;
   end Get_Mouse_Listener;

   --------------
   -- Get_Peer --
   --------------

   function Get_Peer (Obj : in Object) return Peer.Peer is
   begin
      return Obj.My_Peer;
   end Get_Peer;

   ----------
   -- Move --
   ----------

   procedure Move
     (Obj : in out Sized_Object;
      X   : in     Integer;
      Y   : in     Integer)
   is
   begin
      Java.Awt.Component.setLocation(
         Java.Awt.Component.Ref(Obj.My_Peer),
         X,
         Y);
      Java.Awt.Component.setVisible(
         Java.Awt.Component.Ref(Obj.My_Peer),
         true);
   end Move;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Obj    : in out Sized_Object;
      Width  : in     Integer;
      Height : in     Integer)
   is
      Frame_Location   : Java.Awt.Point.Ref;
      Content_Location : Java.Awt.Point.Ref;
   begin
      if Obj.My_Peer.all in Javax.Swing.JFrame.Typ'Class then
         Frame_Location := Java.Awt.Point.Ref(Javax.Swing.JFrame.getLocationOnScreen(
            Javax.Swing.JFrame.Ref(Obj.My_Peer)));

         Content_Location := Java.Awt.Point.Ref(Java.Awt.Container.getLocationOnScreen(
            Javax.Swing.JFrame.getContentPane(
               Javax.Swing.JFrame.Ref(Obj.My_Peer))));
      
         -- add these small numbers to make up for 
         -- the border of the window
         Java.Awt.Component.setSize(
            Java.Awt.Component.Ref(Obj.My_Peer),
            Width + 4,
            Height + Content_Location.Y - Frame_Location.Y + 2);
      else
         Java.Awt.Component.setSize(
            Java.Awt.Component.Ref(Obj.My_Peer),
            Width,
            Height);
      end if;
      Java.Awt.Component.setVisible(
         Java.Awt.Component.Ref(Obj.My_Peer),
         true);
   end Resize;

   --------------------------
   -- Set_Background_Color --
   --------------------------

   procedure Set_Background_Color
     (Obj       : in out Sized_Object;
      New_Color : in     Color)
   is
   begin
      if Obj.My_Peer.all in Javax.Swing.JFrame.Typ'Class then
         Java.Awt.Container.setBackground(
            Javax.Swing.JFrame.getContentPane(
               Javax.Swing.JFrame.Ref(Obj.My_Peer)),
            Java.Awt.Color.new_Color(
               Java.Int(New_Color.Red),
               Java.Int(New_Color.Green),
               Java.Int(New_Color.Blue)));
      elsif Obj.My_Peer.all in Peer.Scrollpane.ListScrollPane_Typ'Class then
         Javax.Swing.JComponent.setBackground(
            Javax.Swing.JComponent.Ref(
               Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer).Listbox),
            Java.Awt.Color.new_Color(
               Java.Int(New_Color.Red),
               Java.Int(New_Color.Green),
               Java.Int(New_Color.Blue)));
      elsif Obj.My_Peer.all in Javax.Swing.JComponent.Typ'Class then
         Javax.Swing.JComponent.setOpaque(
            Javax.Swing.JComponent.Ref(Obj.My_Peer),
            true);
         Javax.Swing.JComponent.setBackground(
            Javax.Swing.JComponent.Ref(Obj.My_Peer),
            Java.Awt.Color.new_Color(
               Java.Int(New_Color.Red),
               Java.Int(New_Color.Green),
               Java.Int(New_Color.Blue)));
      else
         Java.Awt.Component.setBackground(
            Java.Awt.Component.Ref(Obj.My_Peer),
            Java.Awt.Color.new_Color(
               Java.Int(New_Color.Red),
               Java.Int(New_Color.Green),
               Java.Int(New_Color.Blue)));
      end if;
   end Set_Background_Color;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Obj    : in Sized_Object;
      Cursor : in Cursor_Type)
   is
      New_Cursor : Java.Awt.Cursor.Ref;
   begin
      case Cursor is
         when Default_Cursor =>
            New_Cursor := Java.Awt.Cursor.Ref(Java.Awt.Cursor.getDefaultCursor);
         when Resize_NW =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.NW_RESIZE_CURSOR);
         when Resize_N =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.N_RESIZE_CURSOR);
         when Resize_NE =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.NE_RESIZE_CURSOR);
         when Resize_E =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.E_RESIZE_CURSOR);
         when Resize_SE =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.SE_RESIZE_CURSOR);
         when Resize_S =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.S_RESIZE_CURSOR);
         when Resize_SW =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.SW_RESIZE_CURSOR);
         when Resize_W =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.W_RESIZE_CURSOR);
         when Move_Cursor =>
            New_Cursor := Java.Awt.Cursor.new_Cursor(
               Java.Awt.Cursor.MOVE_CURSOR);
      end case;
      Java.Awt.Component.setCursor(
         Java.Awt.Component.Ref(Obj.My_Peer),
         New_Cursor);
   end Set_Cursor;

   ----------------------
   -- Set_Key_Listener --
   ----------------------

   procedure Set_Key_Listener
     (Obj      : in Sized_Object_Pointer;
      Listener : in Key_Listener)
   is
   begin
      if Obj.Key_Handler = null and then Listener /= null then
         Java.Awt.Component.addKeyListener(
            Java.Awt.Component.Ref(Obj.My_Peer),
            Peer.Listeners.new_Key_Adapter(Obj).KeyListener_I);
         Obj.Key_Handler := Listener;
      elsif Obj.Key_Handler /= null and then Listener = null then
         Obj.Key_Handler := Dummy_Key_Listener'access;
      else
         Obj.Key_Handler := Listener;
      end if;
   end Set_Key_Listener;

   ------------------------
   -- Set_Mouse_Listener --
   ------------------------

   procedure Set_Mouse_Listener
     (Obj      : in Sized_Object_Pointer;
      Listener : in Mouse_Listener)
   is
   begin
      if Obj.Mouse_Handler = null and then Listener /= null then
         if Obj.My_Peer.all in Peer.ScrollPane.ListScrollPane_Typ'Class then
            Java.Awt.Component.addMouseListener(
               Java.Awt.Component.Ref(
                  Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer).Listbox),
               Peer.Listeners.new_Mouse_Adapter(Obj).MouseListener_I);
            Java.Awt.Component.addMouseMotionListener(
               Java.Awt.Component.Ref(
                  Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer).Listbox),
               Peer.Listeners.new_Mouse_Motion_Adapter(Obj).MouseMotionListener_I);
         else
            Java.Awt.Component.addMouseListener(
               Java.Awt.Component.Ref(Obj.My_Peer),
               Peer.Listeners.new_Mouse_Adapter(Obj).MouseListener_I);
            Java.Awt.Component.addMouseMotionListener(
               Java.Awt.Component.Ref(Obj.My_Peer),
               Peer.Listeners.new_Mouse_Motion_Adapter(Obj).MouseMotionListener_I);
         end if;
         Obj.Mouse_Handler := Listener;
      elsif Obj.Mouse_Handler /= null and then Listener = null then
         Obj.Mouse_Handler := Dummy_Mouse_Listener'access;
      else
         Obj.Mouse_Handler := Listener;
      end if;
   end Set_Mouse_Listener;

end Mcc.Gui;

