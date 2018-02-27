-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC GUI PACKAGE LIBRARY                               
--           Copyright (C) 1999 Martin C. Carlisle.                
--                                                                 
-- RAPID is free software;  you can  redistribute it  and/or modify
-- it under terms of the  GNU General Public License as published  
-- by the Free Software  Foundation;  either version 2,  or (at your
-- option) any later version.  RAPID is distributed in the hope that 
-- it will be useful, but WITHOUT ANY WARRANTY;  without even the 
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU General Public License for more details.  
-- You should have  received  a copy of the GNU General Public License
-- distributed with RAPID; see file COPYING.  If not, write to the 
-- Free Software Foundation,  59 Temple Place - Suite 330,  Boston, 
-- MA 02111-1307, USA.                                                 
--                                                                     
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an 
-- executable, this unit does not by itself cause the resulting 
-- executable to be covered by the GNU General Public License.  
-- This exception does not however invalidate any other reasons 
-- why the executable file might be covered by the GNU Public 
-- License.  This exception does not apply to executables which
-- are GUI design tools, or that could act as a replacement
-- for RAPID.
------------------------------------------------------------------------------
with Mcc.Gui.Container;
package Mcc.Gui.Widget is
   type Widget is abstract new Sized_Object with private;
   type Widget_Pointer is access all Widget'Class;
   
   ------------------------------------------------
   -- procedure Set_Foreground_Color
   --
   -- change the foreground color for a widget
   ------------------------------------------------
   procedure Set_Foreground_Color(
      Obj       : in out Widget;
      New_Color : in     Color);

   ------------------------------------------------
   -- procedure Set_Tooltip_Text
   --
   -- Set tooltip text for a widget
   -- Widget has no tooltip unless this procedure
   -- has been called.
   ------------------------------------------------
   procedure Set_Tooltip_Text(
      Obj  : in out Widget;
      Text : in String);

   ------------------------------------------------
   -- function Get_Parent
   ------------------------------------------------
   function Get_Parent(Obj : in Widget'Class) 
      return Mcc.Gui.Container.Container_Pointer;
private
   type Widget is abstract new Sized_Object with record
      Parent : Mcc.Gui.Container.Container_Pointer;
   end record;
end Mcc.Gui.Widget;