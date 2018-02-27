---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-PICTURE.ADB
--  Description : GUI Widget Picture Picture
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
with Picture_Dialog_Window;
with Mcc.Gui.Widget.Picture;
with Mcc.Gui.Widget.Label;
use type Mcc.Gui.Widget.Widget_Pointer;
with Generate_Helpers;

package body Gui.Widget.Picture is
   -- reads information from file into Picture,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Picture;
         File : in Ada.Text_Io.File_Type) is

      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(GUI_Widget(Widget),File);

      File_Helpers.Get_String(File,Word,Last);
      Widget.Picture := new String'(Word(1..Last));
   end Read_Widget;

   -- Writes information to file from Picture
   procedure Write_Widget(Widget : in Picture;
         File : in Ada.Text_Io.File_Type) is

   begin -- Write_Widget
      Gui_Enum.IO.Put(File,Gui_Enum.Picture);
      Ada.Text_Io.Put(File," ");
      Write_Widget(GUI_Widget(Widget),File);
      Ada.Text_Io.Put_Line(File," """ & Widget.Picture.all & """ ");
   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Picture;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Picture;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Image;");         
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Picture;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all  &
                 " : aliased Mcc.Gui.Widget.Picture.Picture;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & 
            Generate_Helpers.Undot_Name(Widget.Picture.all) & "_Image" &
               " : aliased Mcc.Gui.Image.External_Image;");                 
   end Generate_Widget_Declaration;
   
   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Picture;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Image.Create(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Obj  => " & 
            Generate_Helpers.Undot_Name(Widget.Picture.all) & "_Image,");

      Ada.Text_IO.Put_Line(File => File,
         Item => "         Name => " & """" & Widget.Picture.all & """);");
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Picture.Create(");
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
         Item => "         Image  => " & 
            Generate_Helpers.Undot_Name(Widget.Picture.all) & "_Image);");
   end Generate_Widget_Creation;


   -- display the widget to a window
   procedure Display_Widget(Widget : in out Picture;
      Container : in out Mcc.Gui.Container.Container'Class) is
      Excepted : Boolean := False;
   begin
      if Widget.The_Widget = null or else
         not (Widget.The_Widget.all in 
         Mcc.Gui.Widget.Picture.Picture'Class) then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Picture.Picture;
      end if;
      
      begin
         Mcc.Gui.Image.Create(Widget.Image,Widget.Picture.all);
      exception when others =>
         Excepted := True;
      end;
      
      if not Excepted then
         Mcc.Gui.Widget.Picture.Create(
            Obj    => Mcc.Gui.Widget.Picture.Picture(
               Widget.The_Widget.all),
            Parent => Container,
            X      => Widget.X,
            Y      => Widget.Y,
            Width  => Widget.Width,
            Height => Widget.Height,
            Image  => Widget.Image);
      else
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Label.Label;
         Mcc.Gui.Widget.Label.Create(
            Obj    => Mcc.Gui.Widget.Label.Label(
               Widget.The_Widget.all),
            Parent => Container,
            X      => Widget.X,
            Y      => Widget.Y,
            Width  => Widget.Width,
            Height => Widget.Height,
            Justify => Mcc.Gui.Widget.Label.Center,
            Text   => Widget.Picture.all);
      end if;

      Display_Widget(Gui_Widget(Widget),Container);
   exception when others =>
      Mcc.Common_Dialogs.OK_Box("Can't display: " &
         Widget.Name.all);
   end Display_Widget;


   procedure Set_Properties(Widget : in out Picture) is
   begin
      Picture_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Picture_Dialog_Window.Picture_Dialog_Window'access;
      Widget.Name_Entry    := Picture_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Picture_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Picture_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Picture_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Picture_Dialog_Window.Entry5'access;
      Widget.Picture_Entry := Picture_Dialog_Window.Entry6'access;
      Set_Properties(Gui_Widget(Widget));
      if Widget.Picture /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Picture_Entry.all,
            Text => Widget.Picture.all);
      end if;
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Picture) is
   begin
      Apply_Properties(Gui_Widget(Widget));
      declare
         Picture : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Picture_Entry.all);
      begin
         Widget.Picture := new String'(Picture);
      end;
   end Apply_Properties;


   procedure Check_Properties(Widget : in out Picture;
      Ok : out Boolean) is
   begin
      Check_Properties(Gui_Widget(Widget),Ok);
      if Ok then
         Ok := (Widget.Picture /= null) and then (Widget.Picture.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Picture_Entry.all);
         end if;
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;


end Gui.Widget.Picture;