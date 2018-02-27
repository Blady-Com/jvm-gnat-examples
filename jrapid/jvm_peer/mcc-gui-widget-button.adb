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
with Java_Strings;
with Javax.Swing.AbstractButton;
with Javax.Swing.ButtonModel;
with Peer.Listeners;
with Peer.Container;
package body Mcc.Gui.Widget.Button is
   -------------
   -- Depress --
   -------------

   procedure Depress (Obj : in out Button) is
   begin
      Javax.Swing.ButtonModel.setPressed(
         Javax.Swing.AbstractButton.getModel(
            Javax.Swing.AbstractButton.Ref(Obj.My_Peer)),
         true);
   end Depress;

   -------------
   -- Disable --
   -------------

   procedure Disable (Obj : in out Button) is
   begin
      Javax.Swing.AbstractButton.setEnabled(
         Javax.Swing.AbstractButton.Ref(Obj.My_Peer),
         false);
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Obj : in out Button) is
   begin
      Javax.Swing.AbstractButton.setEnabled(
         Javax.Swing.AbstractButton.Ref(Obj.My_Peer),
         true);
   end Enable;

   -----------------------------------------
   -- Since I can't remove a Java KeyListener
   -- once I've registered it (I don't keep a 
   -- pointer anywhere), I instead have it
   -- run these dummy procedures.  In my Get
   -- selectors, I catch a pointer to these
   -- and return null instead
   -----------------------------------------
   procedure Dummy_Callback (Obj : in out Button'Class) is
   begin
      null;
   end Dummy_Callback;
   
   -----------------------
   -- Get_Push_Callback --
   -----------------------

   function Get_Push_Callback
     (Obj : in Button'Class)
      return Push_Callback
   is
   begin
      if Obj.Push_Handler /= Dummy_Callback'access then
         return Obj.Push_Handler;
      else
         return null;
      end if;
   end Get_Push_Callback;

   -------------
   -- Release --
   -------------

   procedure Release (Obj : in out Button) is
   begin
      Javax.Swing.ButtonModel.setPressed(
         Javax.Swing.AbstractButton.getModel(
            Javax.Swing.AbstractButton.Ref(Obj.My_Peer)),
         true);
   end Release;

   -----------------------
   -- Set_Push_Callback --
   -----------------------

   procedure Set_Push_Callback
     (Obj           : in Button_Pointer;
      Callback      : in Push_Callback)
   is
   begin
      if Callback /= null and Obj.Push_Handler = null then
         Javax.Swing.AbstractButton.addActionListener(
            Javax.Swing.AbstractButton.Ref(Obj.My_Peer),
            Peer.Listeners.New_Button_Action_Listener(
               Obj).I_ActionListener);
         Obj.Push_Handler := Callback;
      elsif Callback = null and Obj.Push_Handler /= null then
         Obj.Push_Handler := Dummy_Callback'access;
      else
         Obj.Push_Handler := Callback;
      end if;
   end Set_Push_Callback;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Obj  : in out Labeled_Button;
      Text : in     String)
   is
   begin
      Javax.Swing.AbstractButton.setText(
         Javax.Swing.AbstractButton.Ref(Obj.My_Peer),
         Java_Strings.To_Java_String(Text));
   end Set_Text;

end Mcc.Gui.Widget.Button;

