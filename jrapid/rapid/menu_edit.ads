---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  MENU_EDIT.ADS 
--  Description : Used to visually edit the menus
--                       
--  By: Martin Carlisle
--                                         
-- RAPID is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- RAPID is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
-- These procedures happen when the user presses the menu 
-- edit button.  First called is edit, which creates the dialog
---------------------------------------------------------------

with Gui.Menu;
with Mcc.Gui.Container.Window;
with Mcc.Gui.Widget.Button;

package Menu_Edit is

   -- pops up a window that Edits the menu
   procedure Edit(
      Menubar : in Gui.Menu.Menu_Pointer;
      Window  : in Mcc.Gui.Container.Container'Class);

   -- callbacks, each corresponding to a button
   -- on the menu edit window
   procedure Done_Edit;
   procedure Done_Edit(
      Obj : in out Mcc.Gui.Container.Window.Window'Class);
   procedure Done_Edit(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   -- Edit_Item also pops up a dialog
   procedure Edit_Item;
   procedure Edit_Item(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Insert_Menu;
   procedure Insert_Menu(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Insert_Choice;
   procedure Insert_Choice(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   -- what name do we use to display the separator?
   Separator_Name : aliased String := "--------";

   procedure Insert_Separator(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Delete;
   procedure Delete(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   
   -- callbacks corresponding to buttons for when a menu is
   -- inserted or edited.
   procedure Done_Insert_Dialog; -- also used for edit
   procedure Done_Insert_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   procedure Cancel_Insert_Dialog; -- also used for edit
   procedure Cancel_Insert_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   procedure Cancel_Insert_Dialog(
      Obj : in out Mcc.Gui.Container.Window.Window'Class);

end Menu_Edit;