---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  WIDGET_IO.ADS 
--  Description : Read/Write Widgets to file
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
-- This package exists so that it can be called from Gui.Window
-- It traverses the list of widgets calling the appropriate
-- method for each.
---------------------------------------------------------------
with Gui.Widget;
with Ada.Text_IO;
with Gui.Window;

package Widget_IO is
   Bad_File : exception;

   -- for each widget in file:
   -- read widget type
   -- create widget
   -- call Read_Widget method
   -- add to list
   procedure Read_Widgets(File : Ada.Text_IO.File_Type;
      Window_Name : in String;
      Widget_List : out Gui.Widget.Widget_List);

   -- for each widget in file:
   -- call Write_Widget method
   procedure Write_Widgets(File : Ada.Text_IO.File_Type;
      Widgets : Gui.Widget.Widget_List);

   -- for each widget in file:
   -- call Display_Widget method
   procedure Display_Widgets(
         Window   : in out Gui.Window.GUI_Window;
         Widgets  : in     Gui.Widget.Widget_List);

   -- walk through widget list creating Radio groups
   procedure Generate_Radio_Groups(
      File    : Ada.Text_Io.File_Type;
      Widgets : Gui.Widget.Widget_List);
end Widget_IO;