---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  SUBWINDOW_ACTIONS.ADS 
--  Description : Those actions from events in a subwindow
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
with Mcc.Gui.Container.Window;
with Gui.Widget;

package Subwindow_Actions is

   procedure Add_Widget(startx : in Integer; starty : in Integer;
         endx : in Integer; endy : in Integer);

   procedure Duplicate(Widget : in Gui.Widget.Widget_Access);

   procedure Modify_Widget(Obj : in out Mcc.Gui.Widget.Widget'Class);

   procedure Select_Widget(Obj : in out Mcc.Gui.Widget.Widget'Class);

   procedure Move_Widget(
         Obj    : in out Mcc.Gui.Widget.Widget'Class; 
         startx : in     Integer;
         starty : in     Integer;
         endx   : in     Integer; 
         endy   : in     Integer);

   procedure Resize_Widget(Handle : in String;
         startx : in integer; starty : in integer;
         endx   : in Integer; endy   : in Integer);

   procedure Done_Properties_Dialog;
   procedure Done_Properties_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Apply_Properties_Dialog;
   procedure Apply_Properties_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Cancel_Properties_Dialog;
   procedure Cancel_Properties_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class);
   procedure Cancel_Properties_Dialog(
      Obj : in out Mcc.Gui.Container.Window.Window'Class);
 
   procedure Edit_Menu;
   procedure Edit_Menu(Obj : in out Mcc.Gui.Widget.Button.Button'Class);

   procedure Change_Font(Obj : in out Mcc.Gui.Widget.Button.Button'Class);   
   procedure Change_Font_Done;
end Subwindow_Actions;