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
with Java.Lang.Object;
with Java.Lang.Thread;
with Java_Strings;
with Javax.Swing.JComponent;
with Javax.Swing.JFrame;
with Javax.Swing.RepaintManager;
with Javax.Swing.WindowConstants;
with Peer.Listeners;
with Java.Awt.Component;
with Java.Awt.Container;
with Java.Awt.Dimension;
with Java.Awt.Event.WindowAdapter;
with Java.Awt.Window;
with Java.Awt.Event.WindowListener;
with Java.Awt.Layoutmanager;
with Java.Awt.Event.ComponentListener;
with Java.Awt.Point;
package body Mcc.Gui.Container.Window is
   procedure Common_Create(
      Obj    : in out Window'Class;
      X      : in     Natural;
      Y      : in     Natural;
      Width  : in     Natural;
      Height : in     Natural) is
      Frame            : Javax.Swing.JFrame.Ref;
      Frame_Location   : Java.Awt.Point.Ref;
      Content_Location : Java.Awt.Point.Ref;
      Manager          : Javax.Swing.RepaintManager.Ref;
   begin
      Obj.Resize_Callback := null;
      Obj.Close_Callback := null;
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      
      Frame := Javax.Swing.JFrame.new_JFrame;
      Obj.My_Peer := Java.Lang.Object.Ref(Frame);
      
      -- maybe this will make my painting problems go away
      Manager := Javax.Swing.RepaintManager.Ref(Javax.Swing.RepaintManager.currentManager(
         Java.Awt.Component.Ref(Frame)));
      Javax.Swing.RepaintManager.setDoubleBufferingEnabled(
         Manager,False);
         
      Javax.Swing.JFrame.setBounds(Frame,x,y,Width,Height);
      Javax.Swing.JFrame.setVisible(Frame,true);

      -- since I really would like the size of the content pane
      -- to be width, height (to match Tcl/Tk implementation)
      -- figure out the height of the title bar and adjust appropriately
      Frame_Location := Java.Awt.Point.Ref(Javax.Swing.JFrame.getLocationOnScreen(
         Frame));

      Content_Location := Java.Awt.Point.Ref(Java.Awt.Container.getLocationOnScreen(
         Javax.Swing.JFrame.getContentPane(
            Javax.Swing.JFrame.Ref(Obj.My_Peer))));
      
      Javax.Swing.JFrame.setBounds(Frame,x,y,Width,
         Height + Content_Location.Y - Frame_Location.Y);
      --Javax.Swing.JFrame.show(Frame); -- show doesn't exist in my API ????
      
      -- I don't use layout managers in RAPID at this time
      declare
         Null_Layout : Java.Awt.Layoutmanager.Ref := null;
      begin
         Java.Awt.Container.setLayout(
             Javax.Swing.JFrame.getContentPane(Frame),Null_Layout);
      end;
            
      Javax.Swing.JFrame.setDefaultCloseOperation(
         Javax.Swing.JFrame.Ref(Frame),
         Javax.Swing.WindowConstants.Dispose_On_Close);
      -- The adapter implements a Window Listener
      -- Is this how I am supposed to do the conversion?
      Javax.Swing.JFrame.addWindowListener(
         Frame,
         Peer.Listeners.new_Window_Adapter(Obj'unchecked_access).
            WindowListener_I);
      Javax.Swing.JFrame.addComponentListener(
         Frame,
         Peer.Listeners.new_Component_Adapter(Obj'unchecked_access).
            ComponentListener_I);
            
   end Common_Create;
   
   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Main_Window;
      X      : in     Natural;
      Y      : in     Natural;
      Width  : in     Natural;
      Height : in     Natural)
   is
   begin
      Common_Create(
         Obj    => Obj,
         X      => X,
         Y      => Y,
         Width  => Width,
         Height => Height);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Subwindow;
      X      : in     Natural;
      Y      : in     Natural;
      Width  : in     Natural;
      Height : in     Natural)
   is
   begin
      Common_Create(
         Obj    => Obj,
         X      => X,
         Y      => Y,
         Width  => Width,
         Height => Height);
   end Create;

   ----------------
   -- Event_Loop --
   ----------------

   procedure Event_Loop is
   begin
      -- sleep for 20 mins
      Java.Lang.Thread.Sleep(Java.Long'Last);
   end Event_Loop;

   -----------------------
   -- Get_Close_Handler --
   -----------------------

   function Get_Close_Handler
     (Obj : in Window'Class)
      return Close_Handler
   is
   begin
      return Obj.Close_Callback;
   end Get_Close_Handler;

   ------------------------
   -- Get_Resize_Handler --
   ------------------------

   function Get_Resize_Handler
     (Obj : in Window'Class)
      return Resize_Handler
   is
   begin
      return Obj.Resize_Callback;
   end Get_Resize_Handler;

   ----------
   -- Hide --
   ----------

   procedure Hide (Obj : in Window) is
   begin
      Javax.Swing.JFrame.setVisible(
         Javax.Swing.JFrame.Ref(Obj.My_Peer),
         false);
   end Hide;

   -----------------------
   -- Set_Close_Handler --
   -----------------------

   procedure Set_Close_Handler
     (Obj     : in Window_Pointer;
      Handler : in Close_Handler)
   is
   begin
      if Handler /= null then
         Javax.Swing.JFrame.setDefaultCloseOperation(
            Javax.Swing.JFrame.Ref(Obj.My_Peer),
            Javax.Swing.WindowConstants.Do_Nothing_On_Close);
      else
         Javax.Swing.JFrame.setDefaultCloseOperation(
            Javax.Swing.JFrame.Ref(Obj.My_Peer),
            Javax.Swing.WindowConstants.Dispose_On_Close);
      end if;
      Obj.Close_Callback := Handler;
   end Set_Close_Handler;

   ------------------------
   -- Set_Resize_Handler --
   ------------------------

   procedure Set_Resize_Handler
     (Obj     : in Window_Pointer;
      Handler : in Resize_Handler)
   is
   begin
      Obj.Resize_Callback := Handler;
   end Set_Resize_Handler;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Obj   : in out Window;
      Title : in     String)
   is
   begin
      Javax.Swing.JFrame.setTitle(
         Javax.Swing.JFrame.Ref(Obj.My_Peer),
         Java_Strings.To_Java_String(Title));
   end Set_Title;

   ----------
   -- Show --
   ----------

   procedure Show (Obj : in Window) is
   begin
      Javax.Swing.JFrame.setVisible(
         Javax.Swing.JFrame.Ref(Obj.My_Peer),
         true);
   end Show;

   ------------
   -- To_Top --
   ------------

   procedure To_Top (Obj : in Window) is
   begin
      Javax.Swing.JFrame.toFront(
         Javax.Swing.JFrame.Ref(Obj.My_Peer));
      Javax.Swing.JFrame.requestFocus(
         Javax.Swing.JFrame.Ref(Obj.My_Peer));
   end To_Top;

end Mcc.Gui.Container.Window;

