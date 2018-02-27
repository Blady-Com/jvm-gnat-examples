---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-RADIO_BUTTON.ADS
--  Description : GUI Widget Radio Button 
--                       
--  By: Martin Carlisle and Patrick Maes
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
with Ada.Text_Io;
with Mcc.Gui.Widget.Text_Entry;
with Mcc.Gui.Container;

package Gui.Widget.Radio_Button is
   type Radio_Button is new Gui.Widget.Gui_Widget with record
      Text           : String_Pointer;
      Group          : String_pointer;
      Text_Entry     : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Group_Entry    : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Radio_Button;
      File        : in Ada.Text_Io.File_Type);

   -- Writes information to file from GUI_Widget
   procedure Write_Widget(Widget : in Radio_Button;
      File : in Ada.Text_Io.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Radio_Button;
      File : in Ada.Text_Io.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Radio_Button;
      File : in Ada.Text_Io.File_Type);

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Radio_Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Radio_Button;
      Container : in out Mcc.Gui.Container.Container'Class);

   procedure Set_Properties(Widget : in out Radio_Button);

   procedure Apply_Properties(Widget : in out Radio_Button);

   procedure Check_Properties(Widget : in out Radio_Button;
      Ok : out Boolean);

end Gui.Widget.Radio_Button;