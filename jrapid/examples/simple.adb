---------------------------------------------------------------
--                                                           
--  RAPID DEMO     
--                                                           
--  SIMPLE.ADB 
--  Description : Main procedure 
--                       
--  By: Martin Carlisle
--                                         
-- This is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- This is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
with Main_Window;
with Mcc.Gui.Container.Window;

procedure Simple is
begin
   Main_Window.Generate_Window;
   Mcc.Gui.Container.Window.Event_Loop;
end Simple;
