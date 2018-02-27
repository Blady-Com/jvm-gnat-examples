---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-SCALE.ADB
--  Description : GUI Widget Scale 
--                       
--  By: Martin Carlisle, Patrick Maes and W. Blair Watkinson II
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

with Ada.Text_Io,Ada.Integer_Text_IO;
with File_Helpers;
with Gui_Enum;
with Ada.Characters.Handling;
with Generate_Helpers;
with Mcc.Gui.Widget.Scale;
--with Ada.Exceptions;
--use Ada.Exceptions;

-- debugging
with Mcc.Common_Dialogs;
with Scale_Dialog_Window;

package body Gui.Widget.Scale is
   package Orientation_IO is new Ada.Text_IO.Enumeration_IO(
      Mcc.Gui.Widget.Scale.Scale_Orientation);
   -- reads information from file into GUI_Widget,
   -- assumes keyword already read.

   procedure Read_Widget(Widget : in out Scale;
         File        : in Ada.Text_Io.File_Type) is
   begin -- Read_Widget
      Read_Widget(Gui_Widget(Widget),File);

      Ada.Integer_Text_IO.Get(File,Widget.Min);

      Ada.Integer_Text_IO.Get(File,Widget.Max);
      
      Ada.Integer_Text_IO.Get(File,Widget.By);

      Ada.Integer_Text_IO.Get(File,Widget.Mark_Every);

      Orientation_IO.Get(File,Widget.Orientation);

   end Read_Widget;

   -- Writes information to file from GUI_Widget

   procedure Write_Widget(Widget : in Scale;
         File : in Ada.Text_Io.File_Type) is

   begin -- Write_Widget
      Gui_Enum.Io.Put(File,Gui_Enum.Scale);
      Ada.Text_Io.Put(File," ");
      Write_Widget(Gui_Widget(Widget),File);
      Ada.Text_Io.Put(File," ");
      Ada.Integer_Text_IO.Put(File,Widget.Min,0);
      Ada.Text_Io.Put(File," ");
      Ada.Integer_Text_IO.Put(File,Widget.Max,0);
      Ada.Text_Io.Put(File," ");
      Ada.Integer_Text_IO.Put(File,Widget.By,0);
      Ada.Text_Io.Put(File," ");
      Ada.Integer_Text_IO.Put(File,Widget.Mark_Every,0);
      Ada.Text_Io.Put(File," ");
      Orientation_IO.Put(File,Widget.Orientation);
      Ada.Text_Io.New_Line(File);
   end Write_Widget;

   procedure Generate_Widget_Context_Clause(Widget : in Scale;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Scale;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Scale;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Scale.Scale;");
   end Generate_Widget_Declaration;

   procedure Generate_Widget_Creation(Widget : in Scale;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.Scale.Create(");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Obj        => " & Widget.Name.all & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Parent     => " & Window_Name & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         X          =>" & Integer'Image(Widget.X) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Y          =>" & Integer'Image(Widget.Y) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Width      =>" & Integer'Image(Widget.Width) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Min        =>" & Integer'Image(Widget.Min) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Max        =>" & Integer'Image(Widget.Max) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Mark_Every =>" & Integer'Image(Widget.Mark_Every) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Orientation=> Mcc.Gui.Widget.Scale." & 
           Mcc.Gui.Widget.Scale.Scale_Orientation'Image(Widget.Orientation) &
           ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         By         =>" & Integer'Image(Widget.By) & ");");
   end Generate_Widget_Creation;

   -- display the widget to a window
   procedure Display_Widget(
      Widget      : in out Scale;
      Container   : in out Mcc.Gui.Container.Container'Class) is
      use type Mcc.Gui.Widget.Widget_Pointer;
   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Scale.Scale;
      end if;

      Mcc.Gui.Widget.Scale.Create(
         Obj         => Mcc.Gui.Widget.Scale.Scale(
            Widget.The_Widget.all),
         Parent      => Container,
         X           => Widget.X,
         Y           => Widget.Y,
         Width       => Widget.Width,
         Height      => Widget.Height,
         Min         => Widget.Min,
         Max         => Widget.Max,
         Mark_Every  => Widget.Mark_Every,
         Orientation => Widget.Orientation,
         By          => Widget.By);
      Display_Widget(Gui_Widget(Widget),Container);
   exception when others =>
         Mcc.Common_Dialogs.Ok_Box("Can't display: " &
            Widget.Name.all);
   end Display_Widget;

   procedure Set_Properties(Widget : in out Scale) is
      use Mcc.Gui.Widget.Scale;
   begin
      scale_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Scale_Dialog_Window.Scale_Dialog_Window'access;
      Widget.Name_Entry    := Scale_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Scale_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Scale_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Scale_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Scale_Dialog_Window.Entry5'access;
      Widget.Min_Entry     := Scale_Dialog_Window.Entry6'access;
      Widget.Max_Entry     := Scale_Dialog_Window.Entry7'access;
      Widget.By_Entry      := Scale_Dialog_Window.Entry8'access;
      Widget.Mark_Entry    := Scale_Dialog_Window.Entry9'access;
      Widget.Orient_Group  := Scale_Dialog_Window.Orientation'access;
      Widget.Horiz_Radio   := Scale_Dialog_Window.Horizontal'access;
      Widget.Vert_Radio    := Scale_Dialog_Window.Vertical'access;

      Set_Properties(Gui_Widget(Widget));

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Min_Entry.all,
         Text => Widget.Min);
         
      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Max_Entry.all,
         Text => Widget.Max);

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.By_Entry.all,
         Text => Widget.By);
      
      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Mark_Entry.all,
         Text => Widget.Mark_Every);

      if Widget.Orientation = horizontal then
         Mcc.Gui.Widget.Button.Radio.Select_Radio(Widget.Horiz_Radio.all);
      else
         Mcc.Gui.Widget.Button.Radio.Select_Radio(Widget.Vert_Radio.all);
      end if;

   end Set_Properties;

   procedure Apply_Properties(Widget : in out Scale) is
      use type Mcc.Gui.Widget.Button.Radio.Radio_Pointer;
   begin
      Apply_Properties(Gui_Widget(Widget));

      begin
         Widget.Min := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Min_Entry.all);
      exception
         when others => Widget.Min := Integer'Last;
      end;

      begin
         Widget.Max := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Max_Entry.all);
      exception
         when others => Widget.Max := Integer'Last;
      end;

      begin
         Widget.By := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.By_Entry.all);
      exception
         when others => Widget.By := -99;
      end;

      begin
         Widget.Mark_Every := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Mark_Entry.all);
      exception
         when others => Widget.Mark_Every := -99;
      end;
      
      if Mcc.Gui.Widget.Button.Radio.Get_Selected(Widget.Orient_Group.all) =
         Widget.Horiz_Radio then
         Widget.Orientation := Mcc.Gui.Widget.Scale.Horizontal;
      else
         Widget.Orientation := Mcc.Gui.Widget.Scale.Vertical;
      end if;
   end Apply_Properties;

   procedure Check_Properties(Widget : in out Scale;
         Ok : out Boolean) is
   begin
      Check_Properties(Gui_Widget(Widget),Ok);
      if Ok and then Widget.Min = Integer'Last then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Min_Entry.all);
      end if;

      if Ok and then Widget.Max = Integer'Last then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Max_Entry.all);
      end if;

      if Ok and then Widget.Mark_Every < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Mark_Entry.all);
      end if;

      if Ok and then Widget.By < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.By_Entry.all);
      end if;

      if not Ok then
         Mcc.Gui.Bell;
      end if;
   end Check_Properties;

end Gui.Widget.Scale;
