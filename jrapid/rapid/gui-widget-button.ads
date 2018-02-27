---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-BUTTON.ADS 
--  Description : Root of GUI Widget Button Hierarchy
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
-- Additions to hierarchy
--               GUI_Widget
--                   |
--                 Button
--                /      \
--         Text_Button  Picture_Button
--
-- Button adds action (for press)
-- Text_Button adds Text (label)
-- Picture_Button adds Picture -- name of GIF (without .GIF extension)
---------------------------------------------------------------
with ada.text_io;
with Mcc.Gui.Widget.Text_Entry;
with mcc.Gui.Image;
with Mcc.Gui.Container;

package Gui.Widget.Button is
   type Button is abstract new Gui.Widget.GUI_Widget with record
      Action           : String_Pointer;
      Action_Entry     : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Button;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from GUI_Widget
   procedure Write_Widget(Widget : in Button;
      File : in Ada.Text_IO.File_Type);

   procedure Generate_Action_Context_Clause(Widget : in Button;
      File : in Ada.Text_IO.File_Type);
      
   procedure Generate_Widget_Creation(Widget : in Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   procedure Set_Properties(Widget : in out Button);

   procedure Apply_Properties(Widget : in out Button);

   procedure Check_Properties(Widget : in out Button;
      Ok : out Boolean);

   type Text_Button is new Button with record
      Text       : String_Pointer;
      Text_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Text_Button;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from GUI_Widget
   procedure Write_Widget(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99      
   procedure Generate_Widget_Context_Clause(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99      
   procedure Generate_Widget_Declaration(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99      
   procedure Generate_Action_Context_Clause(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Text_Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Text_Button;
      Container : in out Mcc.Gui.Container.Container'Class);

   procedure Set_Properties(Widget : in out Text_Button);

   procedure Apply_Properties(Widget : in out Text_Button);
   procedure Check_Properties(Widget : in out Text_Button;
      Ok : out Boolean);

   type Picture_Button is new Button with record
      Picture       : String_Pointer;
      Picture_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Image         : Mcc.Gui.Image.External_Image;
      Tooltip       : String_Pointer;
      Tip_Entry     : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
   end record;

   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Picture_Button;
      File        : in Ada.Text_IO.File_Type);

   -- Writes information to file from GUI_Widget
   procedure Write_Widget(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99
   procedure Generate_Action_Context_Clause(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99      
   procedure Generate_Widget_Context_Clause(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 6/6/99      
   procedure Generate_Widget_Declaration(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Picture_Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String);

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Picture_Button;
      Container : in out Mcc.Gui.Container.Container'Class);

   procedure Set_Properties(Widget : in out Picture_Button);

   procedure Apply_Properties(Widget : in out Picture_Button);
   procedure Check_Properties(Widget : in out Picture_Button;
      Ok : out Boolean);

end Gui.Widget.Button;