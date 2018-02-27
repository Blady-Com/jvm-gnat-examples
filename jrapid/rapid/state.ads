---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  STATE.ADS 
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
with Gui.Window;
with Gui.Widget;
with Mcc.Gui;

package state is

   function Allocate_Window return Gui.Window.Window_Pointer;
   procedure Destroy_Window(Window : in out Gui.Window.Window_Pointer);

   function Get_Changed return Boolean;
   function Get_Filename return String;
   function Get_Directory return String;
   function Dialog_Running return Boolean;
   function Font_Dialog_Running return Boolean;

   procedure Set_Filename(Name : in String);
   procedure Set_Directory(Name : in String);
   procedure Set_Changed(Changed : in Boolean);
   procedure Set_Dialog_Running(Running : in Boolean);
   procedure Set_Font_Dialog_Running(Running : in Boolean);

   function Get_Current_Window return Gui.Window.Window_Pointer;
   procedure Set_Current_Window(Window : Gui.Window.Window_Pointer);

   -- return the one item that is selected if only one
   function Get_Selection return Gui.Widget.Widget_Access;

   -- used to track if control key pressed
   procedure Main_Window_Key_Listener(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Key_Event);
      
   function Is_Control_Pressed return Boolean;
   procedure Control_Released;
end state;