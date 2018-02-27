---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI_ENUM.ADS 
--  Description : IO for GUI keywords
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
-- Add something to this file every time you create a new widget
-- To create a new widget, you will then need to add something
-- to the gui-widget hierarchy.
---------------------------------------------------------------
-- Change log:
-- 09/20/99 (mcc) : added Separator
-- 02/16/99 (wbw) : added Scale
---------------------------------------------------------------

with Ada.Text_Io;
with Gui.Widget;

package Gui_Enum is

   type Keyword is (Menubar, Menu, Item, Separator, EndOf, Window, Widgets,
      TextButton, PictureButton, Label, TextEntry, CheckButton,
      RadioButton, Picture, Listbox, Scale, Progress, Dropdown);
   subtype Widget_Keyword is Keyword range TextButton..Keyword'Last;
   package IO is new Ada.Text_IO.Enumeration_IO(Keyword);

   ----------------------------------------------------------
   -- Allocate widget
   --
   -- allocate a widget of the given type and return a pointer
   -- to it.  Does not fill in any fields of the widget
   ----------------------------------------------------------
   function Allocate_Widget(Widget_Type : in Widget_Keyword) return
      Gui.Widget.Widget_Access;

end Gui_Enum;