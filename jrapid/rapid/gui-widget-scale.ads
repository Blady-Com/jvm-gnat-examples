---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-SCALE.ADS 
--  Description : GUI Widget Scale 
--                       
--  By: Martin Carlisle, Patrick Maes, and W. Blair Watkinson II
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
with Mcc.Gui.Widget.Button.Radio;
with Mcc.Gui.Widget.Scale;
with Mcc.Gui.Container;

package Gui.Widget.Scale is

   type Scale is new Gui.Widget.Gui_Widget with record
      Min         : Integer;
      Max         : Integer;
      By          : Integer;
      Mark_Every  : Integer;
      -- Pick either vertical or horizontal (default horizontal)
      Orientation : Mcc.Gui.Widget.Scale.Scale_Orientation :=
         Mcc.Gui.Widget.Scale.Horizontal;
      -- pointers into dialog
      Min_Entry   : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Max_Entry   : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      By_Entry    : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Mark_Entry  : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Orient_Group: Mcc.Gui.Widget.Button.Radio.Radio_Group_Pointer;
      Horiz_Radio : Mcc.Gui.Widget.Button.Radio.Radio_Pointer;
      Vert_Radio  : Mcc.Gui.Widget.Button.Radio.Radio_Pointer;
   end record;

   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget (Widget : in out Scale;
      File        : in Ada.Text_Io.File_Type);

   -- Writes information to file from GUI_Widget
   procedure Write_Widget (Widget : in Scale;
      File : in Ada.Text_Io.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Scale;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Scale;
      File : in Ada.Text_IO.File_Type);
      
   -- wbw 5/10/99
   procedure Generate_Widget_Creation(Widget : in Scale;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   procedure Display_Widget (Widget : in out Scale;
      Container : in out Mcc.Gui.Container.Container'Class);

   procedure Set_Properties(Widget : in out Scale);

   procedure Apply_Properties(Widget : in out Scale);

   procedure Check_Properties(Widget : in out Scale;
      Ok : out Boolean);

end Gui.Widget.Scale;