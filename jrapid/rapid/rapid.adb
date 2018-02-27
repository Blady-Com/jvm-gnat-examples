---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  RAPID.ADB 
--  Description : Main procedure 
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
with Main_Window;
with Toolbar;
with Ada.Exceptions;
with Mcc.Gui.Container.Window;
with Rapid_Helpers;
with Ada.Strings.Unbounded;
with State;
with Gui.Window;
with Mcc.Common_Dialogs;
with File_Menu;

procedure Rapid is

begin
   main_Window.Generate_Window;
   Toolbar.Initialize_Toolbar;
   -- set minimum window size
   Mcc.Gui.Container.Window.Set_Resize_Handler(
      Obj     => main_Window.main_Window'access,
      Handler => Rapid_Helpers.Rapid_Resize_Handler'access);

   Mcc.Gui.Container.Window.Set_Close_Handler(
      Obj     => Main_Window.Main_Window'access,
      Handler => File_Menu.Exit_Choice'access);
   Mcc.Gui.Set_Key_Listener(
      Obj      => Main_Window.Main_Window'access,
      Listener => State.Main_Window_Key_Listener'access);
   Mcc.Gui.Container.Window.Event_Loop;
exception when E:others =>
   declare
      Window   : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Filename : aliased String := "emergency.gui";
      use type Gui.Window.Window_Pointer;
   begin
      if Window /= null then
         Window.Filename := Filename'Unchecked_Access;
         Gui.Window.Write_Window(Window.all);
      end if;
      
   end;
   Mcc.Common_Dialogs.OK_Box(
      "Open window (if any) dumped to emergency.gui" & ASCII.LF &
      "Uncaught exception -- contact author:" &
      Ada.Exceptions.Exception_Information(E));
   raise;
end Rapid;
