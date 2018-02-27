---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-DROPDOWN.ADS
--  Description : GUI Widget Dropdown
--                       
--  By: Jonathan Busch and Martin Carlisle
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
with Mcc.Gui.Container;

package Gui.Widget.Dropdown is

   type Dropdown is new GUI_Widget with record
      FG_Color     : String_Pointer;
      BG_Color     : String_Pointer;
      -- although this really should be a natural, we use
      -- a negative number as an error condition
      Number_Rows  : Integer := 5;
      -- mcc gui components for entry
      FG_Entry        : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      BG_Entry        : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Rows_Entry      : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into Dropdown,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Dropdown;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from Dropdown
   procedure Write_Widget(Widget : in Dropdown;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Dropdown;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Dropdown;
      File : in Ada.Text_IO.File_Type);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Dropdown;
      Container : in out Mcc.Gui.Container.Container'Class);
      
   procedure Generate_Widget_Creation(Widget : in Dropdown;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   procedure Set_Properties(Widget : in out Dropdown);

   procedure Apply_Properties(Widget : in out Dropdown);
   procedure Check_Properties(Widget : in out Dropdown;
      Ok : out Boolean);

end gui.widget.Dropdown;