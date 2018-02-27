-------------------------------------------------------------------
--           GROUP_MANAGER 
--           Copyright (C) 1999 Martin C. Carlisle.                
--                                                                 
-- GROUP_MANAGER is free software; you can redistribute it and/or modify
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
-------------------------------------------------------------------
with Lists_Generic;
with Lists_Generic.Key;
--with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
package body Group_Manager is
--   package SA is new System.Address_To_Access_Conversions(
--      Object => Mcc.Gui.Widget.Button.Radio.Radio_Button'Class
--      );
--   function UC is new Ada.Unchecked_Conversion(SA.Object_Pointer,
--      Mcc.Gui.Widget.Button.Radio.Radio_Pointer);
   type fred is access constant Mcc.Gui.Widget.Button.Radio.Radio_Button;
   function UC is new Ada.Unchecked_Conversion(fred,
      Mcc.Gui.Widget.Button.Radio.Radio_Pointer);
      
   type List_Item is record
      Name  : Gui.String_Pointer;
      Group : Mcc.Gui.Widget.Button.Radio.Radio_Group;
   end record;
   
   function Getkey(X : in List_Item) return String is
   begin
      return X.Name.all;
   end Getkey;

   package Group_List_Package is new Lists_Generic(List_Item);
   package Group_List_Package_Key is new Group_List_Package.Key(
      Getkey);
   
   Group_List : Group_List_Package.List;   
   
   ------------------
   -- Add_To_Group --
   ------------------

   procedure Add_To_Group
     (Obj    : in Mcc.Gui.Widget.Button.Radio.Radio_Button;
      Group  : in Gui.String_Pointer) is
      Item        : List_Item;
      Position    : Group_List_Package.Position;
      Obj_Pointer : Mcc.Gui.Widget.Button.Radio.Radio_Pointer :=
         UC(Obj'Unchecked_Access);
--      Obj_Pointer : Mcc.Gui.Widget.Button.Radio.Radio_Pointer :=
--         UC(SA.To_Pointer(Obj'Address));
   begin
      begin
         Position := Group_List_Package_Key.Find(
            Key => Group.all, 
            Ptr => Group_List);
         Item := Group_List_Package.Retrieve(Group_List,Position);
         Mcc.Gui.Widget.Button.Radio.Add_To_Group(
            Group  => Item.Group,
            Button => Obj_Pointer);
      exception 
         when Group_List_Package_Key.Not_Found =>
            Item.Name := new String'(Group.all);
            -- note it is important to do the add_to_group
            -- before adding to list so that the group is
            -- initialized before it is copied into the list
            Mcc.Gui.Widget.Button.Radio.Add_To_Group(
               Group  => Item.Group,
               Button => Obj_Pointer);
            Group_List_Package.AddToRear(Group_List,Item);
      end;
   end Add_To_Group;
begin
   Group_List_Package.Initialize(Group_List);
end Group_Manager;
