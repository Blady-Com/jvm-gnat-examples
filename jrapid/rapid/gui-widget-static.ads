---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-STATIC.ADS
--  Description : GUI Widget Static Label
--                       
--  By: Martin Carlisle and Jonathan Busch
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

package Gui.Widget.Static is

   type Label is new GUI_Widget with record
      Text          : String_Pointer := NULL;
      Justify	  : String_Pointer := NULL;
      FG_Color	  : String_Pointer := NULL;
      BG_Color	  : String_Pointer := NULL;
      Text_Entry    : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Justify_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      FG_Entry      : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      BG_Entry      : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into Label,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Label;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from Label
   procedure Write_Widget(Widget : in Label;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Label;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Label;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Label;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Label;
      Container : in out Mcc.Gui.Container.Container'Class);

   procedure Set_Properties(Widget : in out Label);

   procedure Apply_Properties(Widget : in out Label);
   procedure Check_Properties(Widget : in out Label;
      Ok : out Boolean);

end gui.widget.static;