---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-PICTURE.ADS
--  Description : GUI Widget Picture (from GIF)
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
with Mcc.Gui.Widget.Text_Entry;
with Mcc.Gui.Container;
with Mcc.Gui.Image;

package Gui.Widget.Picture is

   type Picture is new GUI_Widget with record
      Picture       : String_Pointer := NULL;
      Image         : Mcc.Gui.Image.External_Image;
      Picture_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into Picture,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Picture;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from Picture
   procedure Write_Widget(Widget : in Picture;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Picture;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Picture;
      File : in Ada.Text_IO.File_Type);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Picture;
      Container : in out Mcc.Gui.Container.Container'Class);
      
   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Picture;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   procedure Set_Properties(Widget : in out Picture);

   procedure Apply_Properties(Widget : in out Picture);
   procedure Check_Properties(Widget : in out Picture;
      Ok : out Boolean);

end gui.widget.Picture;