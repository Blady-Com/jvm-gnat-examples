---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-TEXT.ADS
--  Description : GUI Widget Text Box
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
with Mcc.Gui.Container;
with Mcc.Gui.Widget.Text_Entry;

package Gui.Widget.Text is

   type Text_Entry is new GUI_Widget with null record;

   -- reads information from file into Text_Entry,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Text_Entry;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from Text_Entry
   procedure Write_Widget(Widget : in Text_Entry;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Text_Entry;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Text_Entry;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Text_Entry;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Text_Entry;
      Container : in out Mcc.Gui.Container.Container'Class);

   procedure Set_Properties(Widget : in out Text_Entry);

   procedure Apply_Properties(Widget : in out Text_Entry);
   procedure Check_Properties(Widget : in out Text_Entry;
      Ok : out Boolean);

end Gui.Widget.Text;