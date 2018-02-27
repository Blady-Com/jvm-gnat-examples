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
package body Mcc.Gui.Container.Window is

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
      Frame : Javax.Swing.JFrame.Ref;
   begin
      Frame := Javax.Swing.JFrame.new_JFrame;
      Obj.My_Peer := Java.Lang.Object.Ref(Frame);
      Javax.Swing.JFrame.setBounds(Frame,x,y,Width,Height);
      Javax.Swing.JFrame.setVisible(Frame,true);
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
      Frame : Javax.Swing.JFrame.Ref;
   begin
      Frame := Javax.Swing.JFrame.new_JFrame;
      Obj.My_Peer := Java.Lang.Object.Ref(Frame);
      Javax.Swing.JFrame.setBounds(Frame,x,y,Width,Height);
      Javax.Swing.JFrame.setVisible(Frame,true);
   end Create;

   ----------------
   -- Event_Loop --
   ----------------

   procedure Event_Loop is
   begin
      Javax.Swing.JFrame.setVisible(
         Javax.Swing.JFrame.Ref(Obj.My_Peer),
         false);
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
      null;
   end Set_Close_Handler;

   ------------------------
   -- Set_Resize_Handler --
   ------------------------

   procedure Set_Resize_Handler
     (Obj     : in Window_Pointer;
      Handler : in Resize_Handler)
   is
   begin
      null;
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
      Javax.Swing.JFrame.ToFront(
         Javax.Swing.JFrame.Ref(Obj.My_Peer));
   end To_Top;

end Mcc.Gui.Container.Window;

