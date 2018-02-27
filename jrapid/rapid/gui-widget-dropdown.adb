---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-DROPDOWN.ADB
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
with Gui_Enum;
with File_Helpers;
with Mcc.Common_Dialogs;
with Dropdown_Dialog_Window;
with Ada.Exceptions;
with Ada.Text_Io;
with Ada.Integer_Text_Io;
with Mcc.Gui.Widget.Button.Check;
with Mcc.Gui.Widget.Dropdown;
use type Mcc.Gui.Widget.Widget_Pointer;
with Mcc.Gui.Colors;

package body Gui.Widget.Dropdown is

   -- reads information from file into Dropdown,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Dropdown;
         File   : in Ada.Text_Io.File_Type) is

      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(Gui_Widget(Widget),File);
      -- Get number of rows for list when open
      Ada.Integer_Text_Io.Get(File,Widget.Number_Rows);
      
      -- Get Colors
      File_Helpers.Get_String(File,Word,Last);
      Widget.FG_Color := new String'(Word(1..Last));
      File_Helpers.Get_String(File,Word,Last);
      Widget.BG_Color := new String'(Word(1..Last));
   end Read_Widget;


   -- Writes information to file from Dropdown
   procedure Write_Widget(Widget : in Dropdown;
         File   : in Ada.Text_Io.File_Type) is

   begin -- Write_Widget
      Gui_Enum.Io.Put(File,Gui_Enum.Dropdown);
      Ada.Text_Io.Put(File," ");
      Write_Widget(Gui_Widget(Widget),File);

      Ada.Integer_Text_Io.Put(File,Widget.Number_Rows);
      Ada.Text_IO.Put(File," " & Widget.FG_Color.all);
      Ada.Text_IO.Put_Line(File," " & Widget.BG_Color.all & " ");

   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Dropdown;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Dropdown;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Colors;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Dropdown;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Dropdown.Dropdown;");
   end Generate_Widget_Declaration;
   
   -- wbw 5/10/99
   procedure Generate_Widget_Creation(Widget : in Dropdown;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Dropdown.Create(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Obj                  => " & Widget.Name.all & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Parent               => " & Window_Name & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         X                    =>" & Integer'Image(Widget.X) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Y                    =>" & Integer'Image(Widget.Y) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Width                =>" & Integer'Image(Widget.Width) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Height               =>" & Integer'Image(Widget.Height) & ",");           
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Number_Rows          =>" & Integer'Image(Widget.Number_Rows) & ");");
      if Widget.BG_Color.all /= "default" then
         Ada.Text_IO.Put_Line(File => File,
            Item => "      Mcc.Gui.Widget.Dropdown.Set_Background_Color(");
         Ada.Text_IO.Put_Line(File => File,
            Item => "         Obj       => " & Widget.Name.all & ",");
         Ada.Text_IO.Put_Line(File => File,
            Item => "         New_Color => Mcc.Gui.Colors.Named_Color(");
         Ada.Text_IO.Put_Line(File => File,
            Item => "            Mcc.Gui.Colors." &
               Widget.BG_Color.all & "));");
      end if;
      
      if Widget.FG_Color.all /= "default" then
         Ada.Text_IO.Put_Line(File => File,
            Item => "      Mcc.Gui.Widget.Dropdown.Set_Foreground_Color(");
         Ada.Text_IO.Put_Line(File => File,
            Item => "         Obj       => " & Widget.Name.all & ",");
         Ada.Text_IO.Put_Line(File => File,
            Item => "         New_Color => Mcc.Gui.Colors.Named_Color(");
         Ada.Text_IO.Put_Line(File => File,
            Item => "            Mcc.Gui.Colors." & 
               Widget.FG_Color.all & "));");
      end if;
   end Generate_Widget_Creation;


   -- display the widget to a window
   procedure Display_Widget(Widget      : in out Dropdown;
         Container : in out Mcc.Gui.Container.Container'Class) is

   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Dropdown.Dropdown;
      end if;
      
      Mcc.Gui.Widget.Dropdown.Create(
         Obj    => Mcc.Gui.Widget.Dropdown.Dropdown(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height,
         Number_Rows => Widget.Number_Rows);

      if Widget.FG_Color.all /= "default" then
         Mcc.Gui.Widget.Set_Foreground_Color(
            Obj       => Widget.The_Widget.all,
            New_Color => Mcc.Gui.Colors.Named_Color(
               Mcc.Gui.Colors.Color_Enum'Value(Widget.FG_Color.all)));
      end if;
      if Widget.BG_Color.all /= "default" then
         Mcc.Gui.Widget.Set_Background_Color(
            Obj       => Widget.The_Widget.all,
            New_Color => Mcc.Gui.Colors.Named_Color(
               Mcc.Gui.Colors.Color_Enum'Value(Widget.BG_Color.all)));
      end if;
       
      Display_Widget(Gui_Widget(Widget),Container);

   exception when e:others =>
         Mcc.Common_Dialogs.Ok_Box("Can't display: " &
            Widget.Name.all & ascii.lf &
            ada.exceptions.exception_information(e));
   end Display_Widget;


   procedure Set_Properties(Widget : in out Dropdown) is
   begin
      Dropdown_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Dropdown_Dialog_Window.Dropdown_Dialog_Window'access;
      Widget.Name_Entry    := Dropdown_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Dropdown_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Dropdown_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Dropdown_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Dropdown_Dialog_Window.Entry5'access;
      Widget.FG_Entry      := Dropdown_Dialog_Window.Entry6'access;
      Widget.BG_Entry      := Dropdown_Dialog_Window.Entry7'access;
      Widget.Rows_Entry    := Dropdown_Dialog_Window.Row_Entry'access;
      Set_Properties(Gui_Widget(Widget));

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Rows_Entry.all,
         Text => Widget.Number_Rows);

      if Widget.BG_Color /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.BG_Entry.all,
            Text => Widget.BG_Color.all);
      end if;
      if Widget.FG_Color /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.FG_Entry.all,
            Text => Widget.FG_Color.all);
      end if;
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Dropdown) is
   begin
      Apply_Properties(Gui_Widget(Widget));

      begin
         Widget.Number_Rows := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Rows_Entry.all);
      exception
         when others => Widget.Number_Rows := -99;
      end;

      declare
         FG_Color : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.FG_Entry.all);
      begin
         Widget.FG_Color := new String'(FG_Color);
      end;
      declare
         BG_Color : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.BG_Entry.all);
      begin
         Widget.BG_Color := new String'(BG_Color);
      end;

   end Apply_Properties;


   procedure Check_Properties(Widget : in out Dropdown;
         Ok     : out Boolean) is
   begin
      Check_Properties(Gui_Widget(Widget),Ok);

      if Ok and then Widget.Number_Rows < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Rows_Entry.all);
      end if;

      if Ok then
         Ok := (Widget.FG_Color /= null) and then (Widget.FG_Color.all /= "");
         if Ok and then Widget.FG_Color.all /= "default" then
            declare
               Color : Mcc.Gui.Colors.Color_Enum;
            begin
               Color := Mcc.Gui.Colors.Color_Enum'Value(
                  Widget.FG_Color.all);
            exception 
               when others => Ok := False;
            end;   
         end if;
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.FG_Entry.all);
         end if;
      end if;
      if Ok then
         Ok := (Widget.BG_Color /= null) and then (Widget.BG_Color.all /= "");
         if Ok and then Widget.BG_Color.all /= "default" then
            declare
               Color : Mcc.Gui.Colors.Color_Enum;
            begin
               Color := Mcc.Gui.Colors.Color_Enum'Value(
                  Widget.BG_Color.all);
            exception 
               when others => Ok := False;
            end;   
         end if;
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.BG_Entry.all);
         end if;
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;

end Gui.Widget.Dropdown;
