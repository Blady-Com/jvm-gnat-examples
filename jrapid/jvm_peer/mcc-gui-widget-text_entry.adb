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
with Java.Awt.Container;
with Javax.Swing.JTextField;
with Java.Lang.Object;
with Ada.Unchecked_Conversion;
with Java_Strings;
with Peer.Container;
package body Mcc.Gui.Widget.Text_Entry is

   -----------
   -- Clear --
   -----------

   procedure Clear (Obj  : in out Text_Entry) is
   begin
      Set_Text(Obj,"");
   end Clear;

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Text_Entry;
      Parent : in     Mcc.Gui.Container.Container'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Width  : in     Natural;
      Height : in     Natural)
   is
      Text_Entry : Javax.Swing.JTextField.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      
      Text_Entry := Javax.Swing.JTextField.new_JTextField;
         
      Obj.My_Peer := Java.Lang.Object.Ref(Text_Entry);

      Javax.Swing.JTextField.setBounds(Text_Entry,x,y,Width,Height);
      
      Peer.Container.Add_To_Parent(Parent,Text_Entry);
      Obj.Parent := Convert(Parent'Unchecked_Access);
   end Create;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Obj : in Text_Entry) return String is
      Text : Java.Lang.String.Ref;
   begin
      Text := Java.Lang.String.Ref(Javax.Swing.JTextField.getText(
         Javax.Swing.JTextField.Ref(Obj.My_Peer)));
      return Java_Strings.To_Ada_String(Text);
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Obj : in Text_Entry) return Integer is
   begin
      return integer'value(Get_Text(Obj));
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Obj : in Text_Entry) return Float is
   begin
      return float'value(Get_Text(Obj));
   end Get_Text;

   ---------------------
   -- Get_Text_Length --
   ---------------------

   function Get_Text_Length (Obj : in Text_Entry) return Natural is
      Text : Java.Lang.String.Ref;
   begin
      Text := Java.Lang.String.Ref(Javax.Swing.JTextField.getText(
         Javax.Swing.JTextField.Ref(Obj.My_Peer)));
      return Natural(Java.Lang.String.length(
         Text));
   end Get_Text_Length;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Obj : in out Text_Entry) is
   begin
      Javax.Swing.JTextField.selectAll(
         Javax.Swing.JTextField.Ref(Obj.My_Peer));
   end Highlight;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight
     (Obj   : in out Text_Entry;
      Start : in     Natural;
      Stop  : in     Natural)
   is
   begin
      Javax.Swing.JTextField.select_K(
         Javax.Swing.JTextField.Ref(Obj.My_Peer),
         start,
         stop);
   end Highlight;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Obj  : in out Text_Entry;
      Text : in     String)
   is
   begin
      Javax.Swing.JTextField.setText(
         Javax.Swing.JTextField.Ref(Obj.My_Peer),
         Java_Strings.To_Java_String(Text));
   end Set_Text;

   ------------------------------------------------
   -- procedure Set_Text
   --
   -- Set text for a text entry using
   -- image of integer (w/o leading spaces)
   ------------------------------------------------
   procedure Set_Text(
      Obj  : in out Text_Entry;
      Text : in     Integer) is
      -- return image of integer w/o leading space
      function integer_image(x : in integer) return String is
         result : string := integer'image(x);
      begin
         if result(result'first) = ' ' then
            return result(result'first+1..result'last);
         else
            return result;
         end if;
      end integer_image;
   begin
      Set_Text(Obj,Integer_Image(Text));
   end Set_Text;

end Mcc.Gui.Widget.Text_Entry;

