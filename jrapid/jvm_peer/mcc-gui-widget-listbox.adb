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
with Javax.Swing.DefaultListModel;
with Javax.Swing.ListModel;
with Javax.Swing.JList;
with Javax.Swing.JScrollPane;
with Javax.Swing.ScrollPaneConstants;
with Peer.Scrollpane;
with Java.Lang.Object;
with Java.Awt.Container;
with Java.Awt.Component;
with Peer.Container;
package body Mcc.Gui.Widget.Listbox is
   function Convert is new Ada.Unchecked_Conversion(
      Javax.Swing.ListModel.Ref,
      Javax.Swing.DefaultListModel.Ref);
   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Obj      : in out Listbox;
      Location : in     Natural;
      Text     : in     String)
   is
      Model      : Javax.Swing.DefaultListModel.Ref;
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
   begin
      Scrollpane := Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer);
      Model := Convert(Javax.Swing.JList.getModel(
         Scrollpane.Listbox));
      Javax.Swing.DefaultListModel.insertElementAt(
         Model,
         Java.Lang.Object.Ref(
            Java_Strings.To_Java_String(Text)),
         Location);
   end Add_Entry;

   -----------
   -- Clear --
   -----------

   procedure Clear (Obj  : in out Listbox) is
      Model      : Javax.Swing.DefaultListModel.Ref;
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
   begin
      Scrollpane := Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer);
      Model := Convert(Javax.Swing.JList.getModel(
         Scrollpane.Listbox));
      Javax.Swing.DefaultListModel.clear(
         Model);
   end Clear;

   -----------
   -- Count --
   -----------

   function Count (Obj : in Listbox) return Natural is
      Model      : Javax.Swing.DefaultListModel.Ref;
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
   begin
      Scrollpane := Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer);
      Model := Convert(Javax.Swing.JList.getModel(
         Scrollpane.Listbox));
      return Javax.Swing.DefaultListModel.getSize(
         Model);
   end Count;

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj                  : in out Listbox;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural;
      Horizontal_Scrollbar : in     Boolean;
      Vertical_Scrollbar   : in     Boolean)
   is
      Listbox    : Javax.Swing.JList.Ref;
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
      Model      : Javax.Swing.DefaultListModel.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
      vertical_policy   : Java.int;
      horizontal_policy : Java.int;
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      
      if Horizontal_Scrollbar then
         Horizontal_Policy := 
            Javax.Swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS;
      else
         Horizontal_Policy := 
            Javax.Swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
      end if;
      
      if Vertical_Scrollbar then
         Vertical_Policy := 
            Javax.Swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;
      else
         Vertical_Policy := 
            Javax.Swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER;
      end if;

      Model := Javax.Swing.DefaultListModel.new_DefaultListModel;
      Listbox := Javax.Swing.JList.new_JList(Model.ListModel_I);
      Scrollpane := Peer.Scrollpane.new_ListScrollPane(
         Listbox,
         vertical_policy,
         horizontal_policy);
         
      Obj.My_Peer := Java.Lang.Object.Ref(Scrollpane);

      Javax.Swing.JScrollPane.setBounds(
         Javax.Swing.JScrollPane.Ref(Scrollpane),
         x,y,Width,Height);
      
      Peer.Container.Add_To_Parent(Parent,Scrollpane);
      Obj.Parent := Convert(Parent'Unchecked_Access);
   end Create;

   ------------------
   -- Delete_Entry --
   ------------------

   procedure Delete_Entry
     (Obj   : in out Listbox;
      Start : in     Natural;
      Stop  : in     Natural)
   is
      Model      : Javax.Swing.DefaultListModel.Ref;
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
   begin
      Scrollpane := Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer);
      Model := Convert(Javax.Swing.JList.getModel(
         Scrollpane.Listbox));
      Javax.Swing.DefaultListModel.removeRange(
         Model,
         start-1,
         stop-1);
   end Delete_Entry;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected (Obj : in Listbox) return Natural is
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
   begin
      Scrollpane := Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer);
      return Javax.Swing.JList.GetSelectedIndex(Scrollpane.Listbox)+1;
   end Get_Selected;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Obj    : in out Listbox;
      Number : in     Natural)
   is
      Scrollpane : Peer.ScrollPane.ListScrollPane_Ref;
   begin
      Scrollpane := Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer);
      Javax.Swing.JList.setSelectedIndex(Scrollpane.Listbox,Number-1);
   end Select_Item;

end Mcc.Gui.Widget.Listbox;

