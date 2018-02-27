---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  FONT_ACTIONS.ADS 
--  Description : Actions while selecting a font
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
with Mcc.Gui.Widget.Button;
with Mcc.Gui.Container.Window;
package Font_Actions is
   -- called to start font selection dialog
   procedure Change_Font(The_Widget  : in Gui.Widget.Widget_Access);

   procedure Ok_Pressed(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Default_Font(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Cancel_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   procedure Cancel_Dialog(
      Obj : in out Mcc.Gui.Container.Window.Window'Class);

   -- make a nice display of a string which is the result of
   -- using 'image on an enumeration type.
   -- This really shouldn't live here, but it is where I
   -- first used it.
   function Display_Enum(Item : in String) return String;
end Font_Actions;