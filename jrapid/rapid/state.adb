---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  STATE.ADB
--  Description : Things to keep track of
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
with Ada.Unchecked_Deallocation;
with Gui,Gui.Window;
with Mcc.Gui.Keys;
--with Mcc.Common_Dialogs; -- for debugging
--with ada.text_io; -- for debugging

package body state is
   Changed_Since_Save : Boolean := False;
   Dialog_Up          : Boolean := False;
   Font_Dialog_Up          : Boolean := False;
   Filename           : String(1..255) := "Untitled" & string'(9..255 => ' ');
   Filename_Last      : Natural := 8;
   Directory          : String(1..255);
   Directory_Last     : Natural;
   Current_Window     : Gui.Window.Window_Pointer := NULL;
   Current_Selection  : Gui.Widget.Widget_Access := NULL;

   function Allocate_Window return Gui.Window.Window_Pointer is

      tempvar : Gui.Window.Window_Pointer;

   begin -- Allocate_Window
      tempvar := new Gui.Window.GUI_Window;
      return tempvar;
   end Allocate_Window;


   procedure Destroy_Window(Window : in out Gui.Window.Window_Pointer) is
      procedure Destroy is new Ada.Unchecked_Deallocation(Gui.Window.GUI_Window,
         Gui.Window.Window_Pointer);
      procedure Destroy is new Ada.Unchecked_Deallocation(String,
         Gui.String_Pointer);
   begin -- Destroy_Window
      Destroy(Window.Window_Name);
      Destroy(Window.Filename);
      -- really should deallocate all of files/widgets here
      Destroy(Window);
   end Destroy_Window;

   function Get_Changed return Boolean is
   begin
      return Changed_Since_Save;
   end Get_Changed;

   function Dialog_Running return Boolean is
   begin
      return Dialog_Up;
   end Dialog_Running;

   function Font_Dialog_Running return Boolean is
   begin
      return Font_Dialog_Up;
   end Font_Dialog_Running;

   function Get_Filename return String is
   begin
      return Filename(1..Filename_Last);
   end Get_Filename;

   function Get_Directory return String is
   begin
      return Directory(1..Directory_Last);
   end Get_Directory;

   procedure Set_Filename(Name : in String) is
   begin
      Filename(1..Name'Length) := Name;
      Filename_Last := Name'Length;
   end Set_Filename;

   procedure Set_Directory(Name : in String) is
   begin
      Directory(1..Name'Length) := Name;
      Directory_Last := Name'Length;
   end Set_Directory;

   procedure Set_Changed(Changed : in Boolean) is
   begin
      Changed_Since_Save := Changed;
   end Set_Changed;

   procedure Set_Dialog_Running(Running : in Boolean) is
   begin
      Dialog_Up := Running;
   end Set_Dialog_Running;

   procedure Set_Font_Dialog_Running(Running : in Boolean) is
   begin
      Font_Dialog_Up := Running;
   end Set_Font_Dialog_Running;

   function Get_Current_Window return Gui.Window.Window_Pointer is
   begin
      return Current_Window;
   end Get_Current_Window;

   procedure Set_Current_Window(Window : Gui.Window.Window_Pointer) is
   begin
      Current_Window := Window;
   end Set_Current_Window;

   function Get_Selection return Gui.Widget.Widget_Access is
      Position    : Gui.Widget.Widget_Pointer;
      Selection   : Gui.Widget.Widget_Access;
      Return_Val  : Gui.Widget.Widget_Access := NULL;
      use type Gui.Widget.Widget_Access;
   begin
      Position := Gui.Widget.Widget_List_Package.First(
         Current_Window.Widget_List);
      while not Gui.Widget.Widget_List_Package.IsPastEnd(
         Current_Window.Widget_List,Position) loop
         Selection := Gui.Widget.Widget_List_Package.Retrieve(
               Current_Window.Widget_List,Position);
         if Selection.Is_Selected then
            if Return_Val = null then
               Return_Val := Selection;
            else
               return null;
            end if;
         end if;
         Gui.Widget.Widget_List_Package.GoAhead(
            Current_Window.Widget_List,Position);
      end loop;
      return Return_Val;
   end Get_Selection;

   -- used to track if control key pressed
   Control_Pressed : Boolean := False;
   procedure Main_Window_Key_Listener(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Key_Event) is
      use Mcc.Gui;
   begin
      if Event.Key = Mcc.Gui.Keys.Control then
         Control_Pressed := (Event.Action = Press);
      end if;
   end Main_Window_Key_Listener;
      
   function Is_Control_Pressed return Boolean is
   begin
      return Control_Pressed;
   end Is_Control_Pressed;
   
   procedure Control_Released is
   begin
      Control_Pressed := False;
   end Control_Released;

end state;