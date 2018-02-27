---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  MENU_GENERATE.ADS
--  Description : Generate Ada code for Menu to file
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

with Gui.Menu;
with Gui.Window;

package Menu_Generate is

   ---------------------------------------------------------------
   -- PROCEDURE Display_Menu_Code
   --
   -- outputs the menu to a window
   ---------------------------------------------------------------
   procedure Display_Menu_Code(
         Menubar   : in     Gui.Menu.Menu_Pointer;
         Window    : in out Gui.Window.GUI_Window;
         Redisplay : in     Boolean := False);
         
end Menu_Generate;