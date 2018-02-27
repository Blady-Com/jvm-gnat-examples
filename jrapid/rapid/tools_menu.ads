---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  TOOLS_MENU.ADS 
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
with Mcc.Gui.Widget.Button;

package Tools_Menu is
   procedure Compile_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   procedure Compile_Choice;
end Tools_Menu;