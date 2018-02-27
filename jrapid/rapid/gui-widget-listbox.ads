---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-LISTBOX.ADS
--  Description : GUI Widget Listbox
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
with Mcc.Gui.Widget.Button.Check;

package Gui.Widget.Listbox is

   type Listbox is new GUI_Widget with record
      HZ_Scroll    : Boolean        := False;
      VR_Scroll    : Boolean        := False;
      FG_Color     : String_Pointer;
      BG_Color     : String_Pointer;
      -- mcc gui components for entry
      HZ_Scroll_Check : Mcc.Gui.Widget.Button.Check.Check_Button_Pointer;
      VR_Scroll_Check : Mcc.Gui.Widget.Button.Check.Check_Button_Pointer;
      FG_Entry        : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      BG_Entry        : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into Listbox,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Listbox;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from Listbox
   procedure Write_Widget(Widget : in Listbox;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in ListBox;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in ListBox;
      File : in Ada.Text_IO.File_Type);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Listbox;
      Container : in out Mcc.Gui.Container.Container'Class);
      
   procedure Generate_Widget_Creation(Widget : in ListBox;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   procedure Set_Properties(Widget : in out Listbox);

   procedure Apply_Properties(Widget : in out Listbox);
   procedure Check_Properties(Widget : in out Listbox;
      Ok : out Boolean);

end gui.widget.listbox;