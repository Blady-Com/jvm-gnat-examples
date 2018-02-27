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
with Javax.Swing.JRadioButton;
with Javax.Swing.JButton;
with Peer.Radio;
with Javax.Swing.ButtonGroup;
with Javax.Swing.AbstractButton;
with Java.Awt.Container;
with Java_Strings;
with Java.Lang.Object;
with Java.Lang.String;
with Java.Awt.Component;
with Java.Awt.ItemSelectable;
with Javax.Swing.ButtonModel;
with Ada.Unchecked_Conversion;
with Peer.Container;
with Java.Util.Vector;

with ada.text_io;
use ada.text_io;
package body Mcc.Gui.Widget.Button.Radio is

   ------------------
   -- Add_To_Group --
   ------------------

   procedure Add_To_Group
     (Group  : in out Radio_Group;
      Button : in     Radio_Pointer)
   is
      use type Java.Lang.Object.Ref;
   begin
      if Group.My_Peer = null then
         Group.My_Peer := Java.Lang.Object.Ref(
            Peer.Radio.new_RadioGroup);
      end if;

--      Javax.Swing.ButtonModel.setGroup(
--         Javax.Swing.AbstractButton.getModel(
--            Javax.Swing.AbstractButton.Ref(Button.My_Peer)),
--         Javax.Swing.ButtonGroup.Ref(Group.My_Peer));
      Java.Util.Vector.addElement(
         Peer.Radio.RadioGroup_Ref(Group.My_Peer).Radios,
         Button.My_Peer);
      Javax.Swing.ButtonGroup.add(
         Javax.Swing.ButtonGroup.Ref(Group.My_Peer),
         Javax.Swing.AbstractButton.Ref(Button.My_Peer));
   end Add_To_Group;

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Radio_Button;
      Parent : in     Mcc.Gui.Container.Container'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Width  : in     Natural;
      Height : in     Natural;
      Text   : in     String)
   is
      Button : Javax.Swing.JRadioButton.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      Obj.Push_Handler := null;
      
      Button := Javax.Swing.JRadioButton.Ref(
         Peer.Radio.new_RadioButton(
            Obj'Unchecked_Access,
            Java_Strings.To_Java_String(Text),
            true));
         
      Obj.My_Peer := Java.Lang.Object.Ref(Button);

      Java.Awt.Component.setBounds(
         Java.Awt.Component.Ref(Button),
         x,y,Width,Height);
      
      Peer.Container.Add_To_Parent(Parent,Button);
      Obj.Parent := Convert(Parent'Unchecked_Access);
   end Create;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected (Group : in Radio_Group) return Radio_Pointer is
      Model    : Javax.Swing.ButtonModel.Ref;
      use type javax.swing.buttonmodel.ref;
      Buttons  : Java.Lang.Object.Arr;
   begin
      Model := 
         Javax.Swing.ButtonGroup.getSelection(
            Javax.Swing.ButtonGroup.Ref(Group.My_Peer));
      if model=null then
        put_line("there is no justice in the world");
      end if;

      Buttons := Java.Util.Vector.toArray(
         Peer.Radio.RadioGroup_Ref(Group.My_Peer).Radios);

      for i in Buttons'range loop
         if Javax.Swing.JRadioButton.isSelected(
            Javax.Swing.JRadioButton.Ref(Buttons(i))) then
            return Peer.Radio.RadioButton_Ref(Buttons(i)).Radio;
         end if;
      end loop;

      return null;
   end Get_Selected;

   ------------------
   -- Select_Radio --
   ------------------

   procedure Select_Radio (Obj : in out Radio_Button) is
   begin
      Javax.Swing.AbstractButton.setSelected(
         Javax.Swing.AbstractButton.Ref(Obj.My_Peer),
         true);
   end Select_Radio;

end Mcc.Gui.Widget.Button.Radio;

