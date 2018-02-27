---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-STATIC.ADB
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
with Gui_Enum;
with File_Helpers;
with Mcc.Common_Dialogs;
with Label_Dialog_Window;
with Mcc.Gui.Widget.Label;
use type Mcc.Gui.Widget.Widget_Pointer;
with Generate_Helpers;
with Mcc.Gui.Colors;
with Ada.Exceptions;

package body Gui.Widget.Static is
   -- reads information from file into Label,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Label;
                         File   : in Ada.Text_IO.File_Type) is

      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(GUI_Widget(Widget),File);

      -- Get subclass of stuff in Widget record added in static ads
      File_Helpers.Get_String(File,Word,Last);
      Widget.Text := new String'(Word(1..Last));

      --Check for justification & colors 
      --  (if statement for RAPID Ver. 1.0 gui files compatibility)
      File_Helpers.Get_String(File,Word,Last);
      if (Last /= Word'First-1) then
         Widget.Justify := new String'(Word(1..Last));

         -- Get Colors
	   File_Helpers.Get_String(File,Word,Last);
         Widget.FG_Color := new String'(Word(1..Last));
         File_Helpers.Get_String(File,Word,Last);
         Widget.BG_Color := new String'(Word(1..Last));
      else
         Widget.Justify  := new String'("left");
         Widget.FG_Color := new String'("default");
         Widget.BG_Color := new String'("default");
      end if;
   end Read_Widget;


   -- Writes information to file from Label
   procedure Write_Widget(Widget : in Label;
                          File   : in Ada.Text_IO.File_Type) is

   begin -- Write_Widget
      Gui_Enum.IO.Put(File,Gui_Enum.Label);
      Ada.Text_IO.Put(File," ");
      Write_Widget(GUI_Widget(Widget),File);
      Ada.Text_IO.Put(File," """);
      File_Helpers.Put_String(File,Widget.Text.all);
      Ada.Text_IO.Put(File,"""");
      Ada.Text_IO.Put(File," " & Widget.Justify.all);
      Ada.Text_IO.Put(File," " & Widget.FG_Color.all);
      Ada.Text_IO.Put_Line(File," " & Widget.BG_Color.all & " ");
   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Label;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Label;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Colors;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Label;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Label.Label;");
   end Generate_Widget_Declaration;

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Label;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Label.Create(");
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
         Item => "         Height  =>" & Integer'Image(Widget.Height) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Justify => Mcc.Gui.Widget.Label." & Widget.Justify.all & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Text    => " & """" & 
            Generate_Helpers.Quote_String(Widget.Text.all) & """);");
      if Widget.BG_Color.all /= "default" then
         Ada.Text_IO.Put_Line(File => File,
            Item => "      Mcc.Gui.Widget.Label.Set_Background_Color(");
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
            Item => "      Mcc.Gui.Widget.Label.Set_Foreground_Color(");
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
      Widget      : in out Label;
      Container   : in out Mcc.Gui.Container.Container'Class) is
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Label.Label;
      end if;

      Mcc.Gui.Widget.Label.Create(
         Obj    => Mcc.Gui.Widget.Label.Label(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height,
         Text   => Widget.Text.all,
         Justify=> Mcc.Gui.Widget.Label.Justification'Value(
            Widget.Justify.all));
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
      Mcc.Common_Dialogs.OK_Box("Can't display: " &
         Widget.Name.all & ASCII.LF &
         Ada.Exceptions.Exception_Information(E));
   end Display_Widget;


   procedure Set_Properties(Widget : in out Label) is
   begin
      Label_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Label_Dialog_Window.Label_Dialog_Window'access;
      Widget.Name_Entry    := Label_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Label_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Label_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Label_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Label_Dialog_Window.Entry5'access;
      Widget.Text_Entry    := Label_Dialog_Window.Entry6'access;
      Widget.Justify_Entry := Label_Dialog_Window.Entry7'access;
      Widget.FG_Entry      := Label_Dialog_Window.Entry8'access;
      Widget.BG_Entry      := Label_Dialog_Window.Entry9'access;

      Widget.Font_Label    := Label_Dialog_Window.Font_Label'access;
      Widget.Style_Label   := Label_Dialog_Window.Font_Style'access;

      Set_Properties(Gui_Widget(Widget));
      if Widget.Text /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Text_Entry.all,
            Text => Widget.Text.all);
      end if;
      if Widget.Justify /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Justify_Entry.all,
            Text => Widget.Justify.all);
      end if;
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

   procedure Apply_Properties(Widget : in out Label) is
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
      declare
         Text : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Text_Entry.all);
      begin
         Widget.Text := new String'(Text);
      end;
      declare
         Justify : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Justify_Entry.all);
      begin
         Widget.Justify := new String'(Justify);
      end;
   end Apply_Properties;


   procedure Check_Properties(Widget : in out Label;
                              Ok     : out Boolean) is
   begin
      Check_Properties(Gui_Widget(Widget),Ok);
      if Ok then
         Ok := (Widget.Text /= null) and then (Widget.Text.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Text_Entry.all);
         end if;
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
      if Ok then
         Ok := (Widget.Justify /= null) and then (Widget.Justify.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Justify_Entry.all);
         end if;
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;

end Gui.Widget.Static;