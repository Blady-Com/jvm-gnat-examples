---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  TOOLBAR.ADS 
--  Description : Manages the widget toolbar
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
-- Implements the toolbar.  When the buttons are pushed,
-- Select_Widget is called.
-- Get_Selected_Widget is used on click and drag to determine
-- what kind of widget is to be added.
--
-- Must call Initialize_Toolbar first (see rapid.adb)
---------------------------------------------------------------
-- modified by C2C W. Blair Watkinson II on 16 Feb 99
-- added constant declaration for "Scale"
with Gui_Enum;
with Mcc.Gui.Widget.Button;
package Toolbar is

   subtype Widget_Names is Gui_Enum.Keyword
      range Gui_Enum.Textbutton..Gui_Enum.Keyword'Last;
   Label         : constant Gui_Enum.Keyword := Gui_Enum.Label;
   Textbutton    : constant Gui_Enum.Keyword := Gui_Enum.Textbutton;
   Picturebutton : constant Gui_Enum.Keyword := Gui_Enum.Picturebutton;
   Textentry     : constant Gui_Enum.Keyword := Gui_Enum.Textentry;
   Check_Button  : constant Gui_Enum.Keyword := Gui_Enum.Checkbutton;
   radio_button  : constant gui_Enum.Keyword := gui_enum.Radiobutton;
   Picture       : constant Gui_Enum.Keyword := Gui_Enum.Picture;
   Listbox       : constant Gui_Enum.Keyword := Gui_Enum.Listbox;
   Scale         : constant Gui_Enum.Keyword := Gui_Enum.Scale;
   Progress      : constant Gui_Enum.Keyword := Gui_Enum.Progress;
   Dropdown      : constant Gui_Enum.Keyword := Gui_Enum.Dropdown;

   procedure Select_Widget(Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   procedure Select_Widget(Widget_Type : Widget_Names);

   function Get_Selected_Widget return Widget_Names;

   -- must call me!
   procedure Initialize_Toolbar;
end Toolbar;