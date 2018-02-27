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
with Javax.Swing.JButton;
with Java.Awt.Container;
with Java.Lang.Object;
with Java.Awt.Component;
with Javax.Swing.AbstractButton;
with Javax.Swing.Icon;
with Ada.Unchecked_Conversion;
with Peer.Container;
with Java.Awt.Color;
package body Mcc.Gui.Widget.Button.Picture is

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Picture_Button;
      Parent : in     Mcc.Gui.Container.Container'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Width  : in     Natural;
      Height : in     Natural;
      Image  : in     Mcc.Gui.Object'Class)
   is
      Button : Javax.Swing.JButton.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
      Color : Java.Awt.Color.Ref;
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      Obj.Push_Handler := null;
      
      Button := Javax.Swing.JButton.new_JButton(
         Javax.Swing.Icon.Ref(Image.My_Peer));
      -- Note, I used to create the button, then set the image,
      -- but the button stayed invisible this way until you clicked it.
      -- Doing this didn't solve the problem, but it made me feel
      -- better
      Obj.My_Peer := Java.Lang.Object.Ref(Button);

      Javax.Swing.JButton.setBounds(Button,x,y,Width,Height);
      Obj.Parent := Convert(Parent'Unchecked_Access);
      Peer.Container.Add_To_Parent(Parent,Button);
      Javax.Swing.JButton.repaint(Button);
   end Create;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Obj   : in out Picture_Button;
      Image : in     Mcc.Gui.Object'Class)
   is
   begin
      Javax.Swing.AbstractButton.setIcon(
         Javax.Swing.AbstractButton.Ref(Obj.My_Peer),
         Javax.Swing.Icon.Ref(Image.My_Peer));
   end Set_Image;

end Mcc.Gui.Widget.Button.Picture;

