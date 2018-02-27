---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-TEXT.ADB
--  Description : GUI Widget Text box
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
with Gui_Enum;
with File_Helpers;
with TextEntry_Dialog_Window;
with Mcc.Common_Dialogs;
with Generate_Helpers;
with Mcc.Gui.Widget;
use type Mcc.Gui.Widget.Widget_Pointer;

package body Gui.Widget.Text is
   -- reads information from file into Text_Entry,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Text_Entry;
         File : in Ada.Text_IO.File_Type) is

   begin -- Read_Widget
      Read_Widget(GUI_Widget(Widget),File);
   end Read_Widget;

   -- Writes information to file from Text_Entry
   procedure Write_Widget(Widget : in Text_Entry;
         File : in Ada.Text_IO.File_Type) is

   begin -- Write_Widget
      Gui_Enum.IO.Put(File,Gui_Enum.TextEntry);
      Ada.Text_IO.Put(File," ");
      Write_Widget(GUI_Widget(Widget),File);
      Ada.Text_IO.New_Line(File);
   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Text_Entry;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Text_Entry;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Text_Entry;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Text_Entry.Text_Entry;");
   end Generate_Widget_Declaration;

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Text_Entry;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Text_Entry.Create(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Obj    => " & Widget.Name.all & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Parent => " & Window_Name & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         X      =>" & Integer'Image(Widget.X) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Y      =>" & Integer'Image(Widget.Y) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Width  =>" & Integer'Image(Widget.Width) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Height =>" & Integer'Image(Widget.Height) & ");");           
   end Generate_Widget_Creation;


   -- display the widget to a window
   procedure Display_Widget(Widget : in out Text_Entry;
      Container : in out Mcc.Gui.Container.Container'Class) is
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Text_Entry.Text_Entry;
      end if;

      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => Mcc.Gui.Widget.Text_Entry.Text_Entry(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height);
      Display_Widget(Gui_Widget(Widget),Container);
   exception when others =>
      Mcc.Common_Dialogs.OK_Box("Can't display: " &
         Widget.Name.all);
   end Display_Widget;

   procedure Set_Properties(Widget : in out Text_Entry) is
   begin
      TextEntry_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Textentry_Dialog_Window.Textentry_Dialog_Window'access;
      Widget.Name_Entry    := TextEntry_Dialog_Window.Entry1'access;
      Widget.X_Entry       := TextEntry_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := TextEntry_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := TextEntry_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := TextEntry_Dialog_Window.Entry5'access;
      Set_Properties(Gui_Widget(Widget));
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Text_Entry) is
   begin
      Apply_Properties(Gui_Widget(Widget));
   end Apply_Properties;

   procedure Check_Properties(Widget : in out Text_Entry;
      Ok : out Boolean) is
   begin
      Check_Properties(GUI_Widget(Widget),Ok);

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;

end Gui.Widget.Text;