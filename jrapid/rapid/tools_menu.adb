---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  TOOLS_MENU.ADB
--  Description : Implements choices from the Tools menu
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
with State;
with Gui.Window;
use type Gui.Window.Window_Pointer;

package body Tools_Menu is
   --------------------------------------------------------------
   -- procedure Compile_Choice
   --
   -- If no window, do nothing
   -- Generate Tcl code
   -- Generate Ada code
   --------------------------------------------------------------
   procedure Compile_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Compile_Choice;
   end Compile_Choice;

   procedure Compile_Choice is

   begin -- Compile_Choice
      if State.Get_Current_Window = NULL then
         return;
      end if;

      if State.Get_Changed then
         null;
      end if;

      Gui.Window.Generate_Window(Window => State.Get_Current_Window.all);
      
   end Compile_Choice;

end Tools_Menu;