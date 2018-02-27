---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-RADIO_BUTTON.ADB
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
with File_Helpers;
with Gui_Enum;
with Radio_Button_Dialog_Window;
with Mcc.Gui.Widget.Text_Entry;
use type Mcc.Gui.Widget.Widget_Pointer;
with Mcc.Gui.Widget.Button.Radio;
with Mcc.Common_Dialogs;
with Group_Manager;
with Generate_Helpers;

package body Gui.Widget.Radio_Button is

   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Radio_Button;
         File        : in Ada.Text_Io.File_Type) is
      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(Gui_Widget(Widget),File);
      File_Helpers.Get_String(File,Word,Last);
      Widget.Text := new String'(Word(1..Last));
      File_Helpers.Get_String(File,Word,Last);
      Widget.Group := new String'(Word(1..Last));
   end Read_Widget;

   -- Writes information to file from GUI_Widget

   procedure Write_Widget(Widget : in Radio_Button;
         File : in Ada.Text_Io.File_Type) is

   begin -- Write_Widget
      Gui_Enum.Io.Put(File,Gui_Enum.Radiobutton);
      Ada.Text_Io.Put(File," ");
      Write_Widget(Gui_Widget(Widget),File);
      Ada.Text_Io.Put(File," """);
      File_Helpers.Put_String(File,Widget.Text.all);
      Ada.Text_Io.Put(File,"""");
      Ada.Text_Io.Put_Line(File," """ & Widget.Group.all & """");
   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Radio_Button;
      File : in Ada.Text_Io.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Button.Radio;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Radio_Button;
      File : in Ada.Text_Io.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Button.Radio.Radio_Button;");
   end Generate_Widget_Declaration;

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Radio_Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Button.Radio.Create(");
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
         Item => "         Height =>" & Integer'Image(Widget.Height) & ",");           
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Text   => """ & 
            Generate_Helpers.Quote_String(Widget.Text.all) & """);");
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Button.Radio.Add_To_Group(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Group    => " & Widget.Group.all & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Button   => " & Widget.Name.all & "'access);");
   end Generate_Widget_Creation;

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Radio_Button;
         Container : in out Mcc.Gui.Container.Container'Class) is
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Button.Radio.Radio_Button;
      end if;

      Mcc.Gui.Widget.Button.Radio.Create(
         Obj    => Mcc.Gui.Widget.Button.Radio.Radio_Button(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height,
         Text   => Widget.Text.all);
      Group_Manager.Add_To_Group(
         Obj    => Mcc.Gui.Widget.Button.Radio.Radio_Button(
            Widget.The_Widget.all),
         Group  => Widget.Group);
         
      Gui.Widget.Display_Widget(
         Widget    => Gui.Widget.GUI_Widget(Widget),
         Container => Container);
         
   exception when others =>
         Mcc.Common_Dialogs.Ok_Box("Can't display: " &
            Widget.Name.all);
   end Display_Widget;

   procedure Set_Properties(Widget : in out Radio_Button) is
   begin
      Radio_Button_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Radio_button_Dialog_Window.Radio_button_Dialog_Window'access;
      Widget.Name_Entry    := Radio_Button_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Radio_Button_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Radio_Button_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Radio_Button_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Radio_Button_Dialog_Window.Entry5'access;
      Widget.Text_Entry    := Radio_Button_Dialog_Window.Entry6'access;
      Widget.Group_Entry   := Radio_Button_Dialog_Window.Entry7'access;

      Widget.Font_Label    := Radio_Button_Dialog_Window.Font_Label'access;
      Widget.Style_Label   := Radio_Button_Dialog_Window.Font_Style'access;

      Set_Properties(Gui_Widget(Widget));
      if Widget.Text /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Text_Entry.all,
            Text => Widget.Text.all);
      end if;
      if Widget.Group /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Group_Entry.all,
            Text => Widget.Group.all);
      end if;
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Radio_Button) is
   begin
      Apply_Properties(Gui_Widget(Widget));
      declare
         Text : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Text_Entry.all);
      begin
         Widget.Text := new String'(Text);
      end;
      declare
         Group : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Group_Entry.all);
      begin
         Widget.Group := new String'(Group);
      end;
   end Apply_Properties;

   procedure Check_Properties(Widget : in out Radio_Button;
         Ok : out Boolean) is
   begin
      Check_Properties(Gui_Widget(Widget),Ok);
      if Ok then
         Ok := (Widget.Text /= null) and then (Widget.Text.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Text_Entry.all);
         end if;
      end if;

      if Ok then
         Ok := (Widget.Group /= null) and then (Widget.Group.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Group_Entry.all);
         end if;
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;

   end Check_Properties;

end Gui.Widget.Radio_Button;