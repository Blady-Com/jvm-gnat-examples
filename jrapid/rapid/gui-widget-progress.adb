---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-PROGRESS.ADB
--  Description : GUI Widget Progress Bar
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
with Mcc.Common_Dialogs;
with Progress_Dialog_Window;
with Mcc.Gui.Widget.Progress;
use type Mcc.Gui.Widget.Widget_Pointer;
with Mcc.Gui.Colors;

package body Gui.Widget.Progress is
   -- reads information from file into Progress,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Progress;
                         File   : in Ada.Text_IO.File_Type) is

      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(GUI_Widget(Widget),File);

      File_Helpers.Get_String(File,Word,Last);
      Widget.FG_Color := new String'(Word(1..Last));
      File_Helpers.Get_String(File,Word,Last);
      Widget.BG_Color := new String'(Word(1..Last));
   end Read_Widget;


   -- Writes information to file from Progress
   procedure Write_Widget(Widget : in Progress;
                          File   : in Ada.Text_IO.File_Type) is

   begin -- Write_Widget
      Gui_Enum.IO.Put(File,Gui_Enum.Progress);
      Ada.Text_IO.Put(File," ");
      Write_Widget(GUI_Widget(Widget),File);
      Ada.Text_IO.Put(File," " & Widget.FG_Color.all);
      Ada.Text_IO.Put_Line(File," " & Widget.BG_Color.all & " ");
   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Progress;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Progress;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Colors;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Progress;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Progress.Progress;");
   end Generate_Widget_Declaration;

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Progress;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Progress.Create(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Obj     => " & Widget.Name.all & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Parent  => " & Window_Name & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         X       =>" & Integer'Image(Widget.X) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Y       =>" & Integer'Image(Widget.Y) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Width   =>" & Integer'Image(Widget.Width) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Height  =>" & Integer'Image(Widget.Height) & ");");
      if Widget.BG_Color.all /= "default" then
         Ada.Text_IO.Put_Line(File => File,
            Item => "      Mcc.Gui.Widget.Progress.Set_Background_Color(");
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
            Item => "      Mcc.Gui.Widget.Progress.Set_Foreground_Color(");
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
   procedure Display_Widget(
      Widget      : in out Progress;
      Container   : in out Mcc.Gui.Container.Container'Class) is
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Progress.Progress;
      end if;

      Mcc.Gui.Widget.Progress.Create(
         Obj    => Mcc.Gui.Widget.Progress.Progress(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height);
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
      Mcc.Gui.Widget.Progress.Set_Progress(
         Obj    => Mcc.Gui.Widget.Progress.Progress(
            Widget.The_Widget.all),
         Amount => 0.5);
      Display_Widget(Gui_Widget(Widget),Container);
   exception when others =>
      Mcc.Common_Dialogs.OK_Box("Can't display: " &
         Widget.Name.all);
   end Display_Widget;


   procedure Set_Properties(Widget : in out Progress) is
   begin
      Progress_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Progress_Dialog_Window.Progress_Dialog_Window'access;
      Widget.Name_Entry    := Progress_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Progress_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Progress_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Progress_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Progress_Dialog_Window.Entry5'access;
      Widget.FG_Entry      := Progress_Dialog_Window.Entry6'access;
      Widget.BG_Entry      := Progress_Dialog_Window.Entry7'access;
      Set_Properties(Gui_Widget(Widget));
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

   procedure Apply_Properties(Widget : in out Progress) is
   begin
      Apply_Properties(Gui_Widget(Widget));

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


   procedure Check_Properties(Widget : in out Progress;
                              Ok     : out Boolean) is
   begin
      Check_Properties(Gui_Widget(Widget),Ok);
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

end Gui.Widget.Progress;