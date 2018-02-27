---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  FILE_MENU.ADS 
--  Description : Implements choices from the File menu
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
-- These procedures are called when the choice of the same 
-- name is selected from the file menu
---------------------------------------------------------------
with Mcc.Gui.Widget.Button;
with Mcc.Gui.Container.Window;
package File_Menu is

  procedure New_Choice;
  procedure New_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);  
  procedure Open_Choice;
  procedure Open_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);   
  procedure Close_Choice;
  procedure Close_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);  
  procedure Save_Choice;
  procedure Save_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);   
  procedure SaveAs_Choice;
  procedure SaveAs_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class); 
  procedure Exit_Choice;
  procedure Exit_Choice(Obj : in out Mcc.Gui.Container.Window.Window'Class); 
  
  procedure New_Done; -- callback for when new dialog is complete
  procedure New_Done(Obj : in out Mcc.Gui.Widget.Button.Button'Class); 
  procedure Cancel_New; -- callback if New dialog killed by user
  procedure Cancel_New(Obj : in out Mcc.Gui.Container.Window.Window'Class);
end File_Menu;