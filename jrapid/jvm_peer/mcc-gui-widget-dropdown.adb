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
with Ada.Unchecked_Conversion;
with Javax.Swing.JCombobox;
with Java.Lang.Object;
with Java.Awt.Container;
with Java.Awt.Component;
with Peer.Container;
package body Mcc.Gui.Widget.Dropdown is
   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Obj      : in out Dropdown;
      Location : in     Natural;
      Text     : in     String)
   is
      ComboBox   : Javax.Swing.JComboBox.Ref;
   begin
      ComboBox := Javax.Swing.JComboBox.Ref(Obj.My_Peer);
      Javax.Swing.JComboBox.insertItemAt(
         ComboBox,
         Java.Lang.Object.Ref(
            Java_Strings.To_Java_String(Text)),
         Location);
   end Add_Entry;

   -----------
   -- Clear --
   -----------

   procedure Clear (Obj  : in out Dropdown) is
      ComboBox   : Javax.Swing.JComboBox.Ref;
   begin
      ComboBox := Javax.Swing.JComboBox.Ref(Obj.My_Peer);
      Javax.Swing.JComboBox.RemoveAllItems(ComboBox);
   end Clear;

   -----------
   -- Count --
   -----------

   function Count (Obj : in Dropdown) return Natural is
      ComboBox   : Javax.Swing.JComboBox.Ref;
   begin
      ComboBox := Javax.Swing.JComboBox.Ref(Obj.My_Peer);
      return Javax.Swing.JComboBox.getItemCount(ComboBox);
   end Count;

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj                  : in out Dropdown;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural;
      Number_Rows          : in     Natural)
   is
      ComboBox   : Javax.Swing.JComboBox.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      

      ComboBox := Javax.Swing.JComboBox.new_JComboBox;
         
      Obj.My_Peer := Java.Lang.Object.Ref(ComboBox);

      Javax.Swing.JComboBox.setBounds(
         Javax.Swing.JComboBox.Ref(ComboBox),
         x,y,Width,Height);
      Javax.Swing.JComboBox.setMaximumRowCount(
         Javax.Swing.JComboBox.Ref(ComboBox),
         Number_Rows);
      
      Peer.Container.Add_To_Parent(Parent,ComboBox);
      Obj.Parent := Convert(Parent'Unchecked_Access);
   end Create;

   ------------------
   -- Delete_Entry --
   ------------------

   procedure Delete_Entry
     (Obj   : in out Dropdown;
      Start : in     Natural;
      Stop  : in     Natural)
   is
      ComboBox   : Javax.Swing.JComboBox.Ref;
   begin
      ComboBox := Javax.Swing.JComboBox.Ref(Obj.My_Peer);
      for i in reverse start-1..stop-1 loop
         Javax.Swing.JComboBox.removeItemAt(
            ComboBox,i);
      end loop;
   end Delete_Entry;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected (Obj : in Dropdown) return Natural is
      ComboBox   : Javax.Swing.JComboBox.Ref;
   begin
      ComboBox := Javax.Swing.JComboBox.Ref(Obj.My_Peer);
      return Javax.Swing.JComboBox.GetSelectedIndex(ComboBox)+1;
   end Get_Selected;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Obj    : in out Dropdown;
      Number : in     Natural)
   is
      ComboBox   : Javax.Swing.JComboBox.Ref;
   begin
      ComboBox := Javax.Swing.JComboBox.Ref(Obj.My_Peer);
      Javax.Swing.JComboBox.setSelectedIndex(
         ComboBox,
         Number-1);
   end Select_Item;

end Mcc.Gui.Widget.Dropdown;

