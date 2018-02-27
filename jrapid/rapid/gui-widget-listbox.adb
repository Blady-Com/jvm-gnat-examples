---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-LISTBOX.ADB
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
with Gui_Enum;
with File_Helpers;
with Mcc.Common_Dialogs;
with Listbox_Dialog_Window;
with Ada.Exceptions;
with Ada.Text_Io;
with Mcc.Gui.Widget.Button.Check;
with Mcc.Gui.Widget.Listbox;
use type Mcc.Gui.Widget.Widget_Pointer;
with Mcc.Gui.Colors;

package body Gui.Widget.Listbox is
   package Boolean_Io is new Ada.Text_Io.Enumeration_Io(Boolean);

   -- reads information from file into Listbox,
   -- assumes keyword already read.
   procedure Read_Widget(Widget : in out Listbox;
         File   : in Ada.Text_Io.File_Type) is

      Word : Word_Type;
      Last : Natural;
   begin -- Read_Widget
      Read_Widget(Gui_Widget(Widget),File);
      -- Check for horizontal scrollbar
      Boolean_Io.Get(File,Widget.Hz_Scroll);
      
      -- Check for vertical scrollbar
      Boolean_Io.Get(File,Widget.Vr_Scroll);

      -- Get Colors
      File_Helpers.Get_String(File,Word,Last);
      Widget.FG_Color := new String'(Word(1..Last));
      File_Helpers.Get_String(File,Word,Last);
      Widget.BG_Color := new String'(Word(1..Last));
   end Read_Widget;


   -- Writes information to file from Listbox
   procedure Write_Widget(Widget : in Listbox;
         File   : in Ada.Text_Io.File_Type) is

   begin -- Write_Widget
      Gui_Enum.Io.Put(File,Gui_Enum.Listbox);
      Ada.Text_Io.Put(File," ");
      Write_Widget(Gui_Widget(Widget),File);

      Ada.Text_Io.Put(File," " & Boolean'Image(Widget.Hz_Scroll));
      Ada.Text_Io.Put(File," " & Boolean'Image(Widget.Vr_Scroll));
      Ada.Text_IO.Put(File," " & Widget.FG_Color.all);
      Ada.Text_IO.Put_Line(File," " & Widget.BG_Color.all & " ");

   end Write_Widget;

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Listbox;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Widget.Listbox;");
      Ada.Text_IO.Put_Line(File => File,
         Item => "with Mcc.Gui.Colors;");
   end Generate_Widget_Context_Clause;

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Listbox;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "   " & Widget.Name.all &
                 " : aliased Mcc.Gui.Widget.Listbox.Listbox;");
   end Generate_Widget_Declaration;
   
   -- wbw 5/10/99
   procedure Generate_Widget_Creation(Widget : in ListBox;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is
   begin
      Ada.Text_IO.Put_Line(File => File,
         Item => "      Mcc.Gui.Widget.ListBox.Create(");
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
         Item => "         Horizontal_Scrollbar =>" & Boolean'Image(Widget.HZ_Scroll) & ",");
      Ada.Text_IO.Put_Line(File => File,
         Item => "         Vertical_Scrollbar   =>" & Boolean'Image(Widget.VR_Scroll) & ");");
      if Widget.BG_Color.all /= "default" then
         Ada.Text_IO.Put_Line(File => File,
            Item => "      Mcc.Gui.Widget.Listbox.Set_Background_Color(");
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
            Item => "      Mcc.Gui.Widget.Listbox.Set_Foreground_Color(");
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
   procedure Display_Widget(Widget      : in out Listbox;
         Container : in out Mcc.Gui.Container.Container'Class) is

   begin
      if Widget.The_Widget = null then
         Widget.The_Widget := new 
            Mcc.Gui.Widget.Listbox.Listbox;
      end if;
      
      Mcc.Gui.Widget.Listbox.Create(
         Obj    => Mcc.Gui.Widget.Listbox.Listbox(
            Widget.The_Widget.all),
         Parent => Container,
         X      => Widget.X,
         Y      => Widget.Y,
         Width  => Widget.Width,
         Height => Widget.Height,
         Horizontal_Scrollbar => Widget.HZ_Scroll,
         Vertical_Scrollbar   => Widget.VR_Scroll);

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


   procedure Set_Properties(Widget : in out Listbox) is
   begin
      Listbox_Dialog_Window.Generate_Window;
      Widget.Properties    := 
         Listbox_Dialog_Window.Listbox_Dialog_Window'access;
      Widget.Name_Entry    := Listbox_Dialog_Window.Entry1'access;
      Widget.X_Entry       := Listbox_Dialog_Window.Entry2'access;
      Widget.Y_Entry       := Listbox_Dialog_Window.Entry3'access;
      Widget.Width_Entry   := Listbox_Dialog_Window.Entry4'access;
      Widget.Height_Entry  := Listbox_Dialog_Window.Entry5'access;
      Widget.VR_Scroll_Check  := Listbox_Dialog_Window.Vertical'access;
      Widget.HZ_Scroll_Check  := Listbox_Dialog_Window.Horizontal'access;
      Widget.FG_Entry      := Listbox_Dialog_Window.Entry6'access;
      Widget.BG_Entry      := Listbox_Dialog_Window.Entry7'access;
      Set_Properties(Gui_Widget(Widget));

      --vertical scrollbar?
      if (Widget.Vr_Scroll = True) then
         Mcc.Gui.Widget.Button.Check.Select_Check(
            Obj => Widget.Vr_Scroll_Check.all);
      else
         Mcc.Gui.Widget.Button.Check.Unselect_Check(
            Obj => Widget.Vr_Scroll_Check.all);
      end if;

      --horizontal scrollbar?
      if (Widget.Hz_Scroll = True) then
         Mcc.Gui.Widget.Button.Check.Select_Check(
            Obj => Widget.Hz_Scroll_Check.all);
      else
         Mcc.Gui.Widget.Button.Check.Unselect_Check(
            Obj => Widget.Hz_Scroll_Check.all);
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

   procedure Apply_Properties(Widget : in out Listbox) is
   begin
      Apply_Properties(Gui_Widget(Widget));

      --Vertical scrollbar?
      Widget.Vr_Scroll := Mcc.Gui.Widget.Button.Check.Is_Checked(
         Widget.Vr_Scroll_Check.all);

      --Horizontal scrollbar?
      Widget.Hz_Scroll := Mcc.Gui.Widget.Button.Check.Is_Checked(
         Widget.Hz_Scroll_Check.all);

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


   procedure Check_Properties(Widget : in out Listbox;
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

end Gui.Widget.Listbox;
