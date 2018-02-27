---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-BUTTON.ADB 
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
-- Change log:
-- 08/21/98 (mcc) : added test to remove ".gif" if specified
---------------------------------------------------------------
with Ada.Text_IO;
with File_Helpers;
with Gui_Enum;
with PictureButton_Dialog_Window;
with TextButton_Dialog_Window;
with Generate_Helpers;

-- debugging
with Mcc.Common_dialogs;
with Mcc.Gui.Widget.Button.Text;
with Mcc.Gui.Widget.Button.Picture;
use type Mcc.Gui.Widget.Widget_Pointer;

package body Gui.Widget.Button is
   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.

   procedure Read_Widget(Widget : in out Button;
         File        : in Ada.Text_IO.File_Type) is
      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(Gui_Widget(Widget),File);

      File_Helpers.Get_String(File,Word,Last);
      Widget.Action := new String'(Word(1..Last));
   end Read_Widget;

   -- Writes information to file from GUI_Widget

   procedure Write_Widget(Widget : in Button;
         File : in Ada.Text_IO.File_Type) is

   begin -- Write_Widget
      Write_Widget(Gui_Widget(Widget),File);
      Ada.Text_IO.Put(File," """);
      File_Helpers.Put_String(File,Widget.Action.all);
      Ada.Text_IO.Put(File,""" ");
   end Write_Widget;

   -- wbw 6/6/99   
   procedure Generate_Action_Context_Clause(Widget : in Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Generate_Helpers.Generate_With(File,Widget.Action.all);
   end Generate_Action_Context_Clause;

   -- wbw 5/10/99
   procedure Generate_Widget_Creation(Widget : in Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Button.Set_Push_Callback(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Obj      => " & Widget.Name.all & "'access,");
      Ada.Text_IO.Put(File => File,
         Item => "         Callback => ");
      Generate_Helpers.Generate_Removed_Parameters(File,Widget.Action.all);
      Ada.Text_IO.Put_Line(File,
         Item => "'access);");
   end Generate_Widget_Creation;

   procedure Set_Properties(Widget : in out Button) is
   begin
      Set_Properties(Gui_Widget(Widget));
      if Widget.Action /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Action_Entry.all,
            Text => Widget.Action.all);
      end if;
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Button) is
   begin
      Apply_Properties(Gui_Widget(Widget));
      declare
         Action : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Action_Entry.all);
      begin
         Widget.Action := new String'(Action);
      end;
   end Apply_Properties;

   procedure Check_Properties(Widget : in out Button;
      Ok : out Boolean) is
   begin
      Check_Properties(GUI_Widget(Widget),Ok);
      if Ok then
         Ok := (Widget.Action /= null) and then (Widget.Action.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Action_Entry.all);
         end if;
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;


   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Text_Button;
         File        : in Ada.Text_IO.File_Type) is
      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(Button(Widget),File);
      File_Helpers.Get_String(File,Word,Last);
      Widget.Text := new String'(Word(1..Last));
   end Read_Widget;

   -- Writes information to file from GUI_Widget

   procedure Write_Widget(Widget : in Text_Button;
         File : in Ada.Text_IO.File_Type) is

   begin -- Write_Widget
      Gui_Enum.IO.Put(File,Gui_Enum.TextButton);
      Ada.Text_IO.Put(File," ");
      Write_Widget(Button(Widget),File);
      Ada.Text_IO.Put(File," """);
      File_Helpers.Put_String(File,Widget.Text.all);
      Ada.Text_IO.Put_Line(File,"""");
   end Write_Widget;

   procedure Generate_Action_Context_Clause(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Generate_Action_Context_Clause(Button(Widget),File);
   end Generate_Action_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Button.Text;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99      
   procedure Generate_Widget_Declaration(Widget : in Text_Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all  &
                 " : aliased Mcc.Gui.Widget.Button.Text.Text_Button;");
   end Generate_Widget_Declaration;

   -- wbw 5/10/99
   procedure Generate_Widget_Creation(Widget : in Text_Button;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Button.Text.Create(");
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
            Generate_Helpers.Quote_String(Text_Button(Widget).Text.all) & 
            """);");
      Generate_Widget_Creation(Button(Widget),File,Window_Name);
   end Generate_Widget_Creation;

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Text_Button;
      Container : in out Mcc.Gui.Container.Container'Class) is
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Button.Text.Text_Button;
      end if;
      
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => Mcc.Gui.Widget.Button.Text.Text_Button(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height,
         Text   => Widget.Text.all);
      
      Display_Widget(Gui_Widget(Widget),Container);
   exception when others =>
      Mcc.Common_Dialogs.OK_Box("Can't display: " &
         Widget.Name.all);
   end Display_Widget;

   procedure Set_Properties(Widget : in out Text_Button) is
   begin
      TextButton_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Textbutton_Dialog_Window.Textbutton_Dialog_Window'access;
      Widget.Name_Entry    := Textbutton_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Textbutton_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Textbutton_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Textbutton_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Textbutton_Dialog_Window.Entry5'access;
      Widget.Action_Entry  := Textbutton_Dialog_Window.Entry6'access;
      Widget.Text_Entry    := Textbutton_Dialog_Window.Entry7'access;

      Widget.Font_Label    := TextButton_Dialog_Window.Font_Label'access;
      Widget.Style_Label   := TextButton_Dialog_Window.Font_Style'access;

      Set_Properties(Button(Widget));
      if Widget.Text /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Text_Entry.all,
            Text => Widget.Text.all);
      end if;
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Text_Button) is
   begin
      Apply_Properties(Button(Widget));
      declare
         Text : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Text_Entry.all);
      begin
         Widget.Text := new String'(Text);
      end;
   end Apply_Properties;

   procedure Check_Properties(Widget : in out Text_Button;
      Ok : out Boolean) is
   begin
      Check_Properties(Button(Widget),Ok);
      if Ok then
         Ok := (Widget.Text /= null) and then (Widget.Text.all /= "");
         if not Ok then
            Mcc.Gui.Widget.Text_Entry.Highlight(Widget.Text_Entry.all);
         end if;
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;


   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.

   procedure Read_Widget(Widget : in out Picture_Button;
         File        : in Ada.Text_IO.File_Type) is
      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(Button(Widget),File);
      File_Helpers.Get_String(File,Word,Last);
      Widget.Picture := new String'(Word(1..Last));
      File_Helpers.Get_String(File,Word,Last);
      if Last >= Word'First then
         Widget.Tooltip := new String'(Word(1..Last));
      else 
         Widget.Tooltip := null;
      end if;
   end Read_Widget;

   -- Writes information to file from GUI_Widget

   procedure Write_Widget(Widget : in Picture_Button;
         File : in Ada.Text_IO.File_Type) is

   begin -- Write_Widget
      Gui_Enum.IO.Put(File,Gui_Enum.PictureButton);
      Ada.Text_IO.Put(File," ");
      Write_Widget(Button(Widget),File);
      Ada.Text_IO.Put(File," """ & Widget.Picture.all & """ ");
      if Widget.Tooltip /= null then
         Ada.Text_IO.Put(File," """);
         File_Helpers.Put_String(File,Widget.Tooltip.all);
         Ada.Text_IO.Put(File,"""");
      end if;
      Ada.Text_IO.New_Line(File);
   end Write_Widget;

   procedure Generate_Action_Context_Clause(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Generate_Action_Context_Clause(Button(Widget),File);
   end Generate_Action_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Button.Picture;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Image;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Picture_Button;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all & 
               " : aliased Mcc.Gui.Widget.Button.Picture.Picture_Button;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Generate_Helpers.Undot_Name(Widget.Picture.all) & "_Image" &
               " : aliased Mcc.Gui.Image.External_Image;");
   end Generate_Widget_Declaration;

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation(Widget : in Picture_Button;
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
         Item => "      Mcc.Gui.Widget.Button.Picture.Create(");
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
      if Widget.Tooltip /= null then
         Ada.Text_IO.Put_Line(File => File,
            Item => "      Mcc.Gui.Widget.Button.Picture.Set_Tooltip_Text(");
         Ada.Text_IO.Put_Line(File => File,
            Item => "         Obj    => " & Widget.Name.all & ",");
         Ada.Text_IO.Put_Line(File => File,
            Item => "         Text  => """ & 
               Generate_Helpers.Quote_String(Widget.Tooltip.all) & """);");
      end if;
      Generate_Widget_Creation(Button(Widget),File,Window_Name);
   end Generate_Widget_Creation;

   -- display the widget to a window
   procedure Display_Widget(Widget : in out Picture_Button;
      Container : in out Mcc.Gui.Container.Container'Class) is
      Excepted : Boolean := False;
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Button.Picture.Picture_Button;
      end if;

      begin
         Mcc.Gui.Image.Create(Widget.Image,Widget.Picture.all);
      exception when others =>
         Excepted := True;
      end;
      
      if not Excepted then      
         Mcc.Gui.Widget.Button.Picture.Create(
            Obj    => Mcc.Gui.Widget.Button.Picture.Picture_Button(
               Widget.The_Widget.all),
            Parent => Container,
            X      => Widget.X,
            Y      => Widget.Y,
            Width  => Widget.Width,
            Height => Widget.Height,
            Image  => Widget.Image);
      else
         Mcc.Gui.Widget.Button.Text.Create(
            Obj    => Mcc.Gui.Widget.Button.Text.Text_Button(
               Widget.The_Widget.all),
            Parent => Container,
            X      => Widget.X,
            Y      => Widget.Y,
            Width  => Widget.Width,
            Height => Widget.Height,
            Text   => Widget.Picture.all);
      end if;

      if Widget.Tooltip /= null then
         Mcc.Gui.Widget.Set_Tooltip_Text(
            Obj  => Widget.The_Widget.all,
            Text => Widget.Tooltip.all);
      end if;
      Display_Widget(Gui_Widget(Widget),Container);
   exception when others =>
      Mcc.Common_Dialogs.OK_Box("Can't display: " &
         Widget.Name.all);
   end Display_Widget;

   procedure Set_Properties(Widget : in out Picture_Button) is
   begin
      PictureButton_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Picturebutton_Dialog_Window.Picturebutton_Dialog_Window'access;
      Widget.Name_Entry    := Picturebutton_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Picturebutton_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Picturebutton_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Picturebutton_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Picturebutton_Dialog_Window.Entry5'access;
      Widget.Action_Entry  := Picturebutton_Dialog_Window.Entry6'access;
      Widget.Picture_Entry := Picturebutton_Dialog_Window.Entry7'access;
      Widget.Tip_Entry     := Picturebutton_Dialog_Window.Entry8'access;

      Set_Properties(Button(Widget));
      if Widget.Picture /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Picture_Entry.all,
            Text => Widget.Picture.all);
      end if;
      if Widget.Tooltip /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Tip_Entry.all,
            Text => Widget.Tooltip.all);
      end if;
   end Set_Properties;

   procedure Apply_Properties(Widget : in out Picture_Button) is
   begin
      Apply_Properties(Button(Widget));
      declare
         Picture : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Picture_Entry.all);
      begin
         Widget.Picture := new String'(Picture);
      end;
      declare
         Tooltip : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Tip_Entry.all);
      begin
         if Tooltip'Length > 0 then
            Widget.Tooltip := new String'(Tooltip);
         else
            Widget.Tooltip := null;
         end if;
      end;
   end Apply_Properties;

   procedure Check_Properties(Widget : in out Picture_Button;
      Ok : out Boolean) is
   begin
      Check_Properties(Button(Widget),Ok);
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


end Gui.Widget.Button;