---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  EDIT_MENU.ADS 
--  Description : Implements choices from the Edit menu
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
-- These are the procedures called when 
-- something is chosen from the Edit menu.
---------------------------------------------------------------
with Mcc.Gui.Widget.Button;

package Edit_Menu is

  -- used to modify window properties (name/size)
  procedure Change_Window; -- Modify window choice
  procedure Delete_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);
  procedure Delete_Choice;
  procedure Duplicate_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class);
  procedure Duplicate_Choice;

  -- These 3 are called from the window generated 
  -- when you modify the window you are editing
  -- You can apply the changes, cancel them, or push 
  -- OK to apply and close the modify window.
  procedure Change_Done;
  procedure Change_Done(Obj : in out Mcc.Gui.Widget.Button.Button'Class);
  procedure Apply_Changes;
  procedure Apply_Changes(Obj : in out Mcc.Gui.Widget.Button.Button'Class);
  procedure Cancel_Changes;
  procedure Cancel_Changes(Obj : in out Mcc.Gui.Widget.Button.Button'Class);


end Edit_Menu;