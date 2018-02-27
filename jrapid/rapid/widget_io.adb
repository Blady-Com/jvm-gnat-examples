---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  WIDGET_IO.ADB
--  Description : Read/Write Widgets to file
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
with Gui.Widget;
use type Gui.Widget.Widget_Pointer;
with Gui_Enum;
use type Gui_Enum.Keyword;
with Lists_Generic;
with Lists_Generic.Key;
with Gui.Widget.Radio_Button;

package body Widget_Io is

   ----------------------------------------------------------
   -- procedure Read_Widgets
   --
   -- initialize widget list
   -- read a keyword
   -- create the appropriate widget or stop on End
   -- read widget and add to list
   -- after stop, verify it said "end widgets"
   ----------------------------------------------------------
   procedure Read_Widgets(File : Ada.Text_Io.File_Type;
         Window_Name : in String;
         Widget_List : out Gui.Widget.Widget_List) is

      New_Widget : Gui.Widget.Widget_Access;
      Keyword    : Gui_Enum.Keyword;
   begin -- Read_Widgets
      Gui.Widget.Widget_List_Package.Initialize(Widget_List);

      loop
         Gui_Enum.Io.Get(File,Keyword);

         exit when Keyword = Gui_Enum.Endof;

         New_Widget := Gui_Enum.Allocate_Widget(Keyword);
         Gui.Widget.Read_Widget(Widget => New_Widget.all,
            File => File);
         Gui.Widget.Widget_List_Package.Addtorear(Widget_List,
            New_Widget);
      end loop;

      Gui_Enum.Io.Get(File,Keyword);
      if Keyword /= Gui_Enum.Widgets then
         raise Bad_File;
      end if;
   exception
      when others =>
         raise Bad_File;
   end Read_Widgets;


   procedure Write_Widgets(File : Ada.Text_Io.File_Type;
         Widgets : Gui.Widget.Widget_List) is
      Traverse : Gui.Widget.Widget_Pointer :=
         Gui.Widget.Widget_List_Package.First(Widgets);
   begin -- Write_Widgets
      Gui_Enum.Io.Put(File,Gui_Enum.Widgets);
      Ada.Text_Io.New_Line(File);
      while not Gui.Widget.Widget_List_Package.Ispastend(
            Widgets,Traverse) loop
         Gui.Widget.Write_Widget(
            Gui.Widget.Widget_List_Package.Retrieve(Widgets,Traverse).all,
            File);
         Gui.Widget.Widget_List_Package.Goahead(Widgets,Traverse);
      end loop;
      Gui_Enum.Io.Put(File,Gui_Enum.Endof);
      Ada.Text_Io.Put(File,' ');
      Gui_Enum.Io.Put(File,Gui_Enum.Widgets);
      Ada.Text_Io.New_Line(File);
   end Write_Widgets;

   procedure Generate_Radio_Groups(
      File    : Ada.Text_Io.File_Type;
      Widgets : Gui.Widget.Widget_List) is
      
      function Get_Group (Group : Gui.String_Pointer) return String is
      begin
         return Group.all;
      end Get_Group;
      
      package Group_List_Package is new Lists_Generic(Gui.String_Pointer);
      package Group_List_Key_Package is new 
         Group_List_Package.Key(Getkey=>Get_Group);
         
      Traverse : Gui.Widget.Widget_Pointer :=
         Gui.Widget.Widget_List_Package.First(Widgets);
      Group_Position : Group_List_Package.Position;
      Widget   : Gui.Widget.Widget_Access;
      Group    : Gui.String_Pointer;
      The_List : Group_List_Package.List;

   begin
      Group_List_Package.Initialize(The_List);
      -- Walk through list accumulating distinct group names
      while not Gui.Widget.Widget_List_Package.Ispastend(Widgets,Traverse) loop
         Widget := Gui.Widget.Widget_List_Package.Retrieve(Widgets,Traverse);
         if Widget.all in Gui.Widget.Radio_Button.Radio_Button then
            Group := Gui.Widget.Radio_Button.Radio_Button(Widget.all).Group;
            if not Group_List_Key_Package.Ispresent(Group.all,The_List) then
               Group_List_Package.Addtorear(The_List,Group);
            end if;
         end if;
         Gui.Widget.Widget_List_Package.Goahead(Widgets,Traverse);
      end loop;
      
      -- Now walk through and output declarations
      Group_Position := Group_List_Package.First(The_List);
      while not Group_List_Package.Ispastend(The_List,Group_Position) loop
         Group := Group_List_Package.Retrieve(The_List,Group_Position);
         Ada.Text_IO.Put_Line(File,
            "   " & Group.all & 
            " : aliased Mcc.Gui.Widget.Button.Radio.Radio_Group;");
         Group_List_Package.Goahead(The_List,Group_Position);         
      end loop;
   end Generate_Radio_Groups;

 
   procedure Display_Widgets(
         Window   : in out Gui.Window.GUI_Window;
         Widgets  : in     Gui.Widget.Widget_List) is
      Traverse : Gui.Widget.Widget_Pointer :=
         Gui.Widget.Widget_List_Package.First(Widgets);
   begin
      while not Gui.Widget.Widget_List_Package.Ispastend(
            Widgets,Traverse) loop
         Gui.Widget.Display_Widget(
            Widget    => Gui.Widget.Widget_List_Package.Retrieve(
               Widgets,Traverse).all,
            Container => Window.Display_Area);
         Gui.Widget.Widget_List_Package.Goahead(Widgets,Traverse);
      end loop;
   end Display_Widgets;

end Widget_Io;