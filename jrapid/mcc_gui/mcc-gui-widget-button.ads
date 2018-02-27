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
package Mcc.Gui.Widget.Button is
   type Button is abstract new Widget with private;
   type Button_Pointer is access all Button'Class;
   ------------------------------------------------
   -- procedure Depress
   --
   -- draw button as depressed
   ------------------------------------------------
   procedure Depress(Obj : in out Button);

   ------------------------------------------------
   -- procedure Release
   --
   -- draw button as released (raised)
   ------------------------------------------------
   procedure Release(Obj : in out Button);

   ------------------------------------------------
   -- procedure Enable
   --
   -- set button state as enabled
   ------------------------------------------------
   procedure Enable(Obj : in out Button);

   ------------------------------------------------
   -- procedure Disable
   --
   -- set button state as disabled
   ------------------------------------------------
   procedure Disable(Obj : in out Button);

   ------------------------------------------------
   -- procedure Set_Push_Callback
   --
   -- Set callback for button push
   ------------------------------------------------
   type Push_Callback is access procedure (Obj : in out Button'Class);
   procedure Set_Push_Callback(
      Obj           : in Button_Pointer;
      Callback      : in Push_Callback);
   function Get_Push_Callback(
      Obj : in Button'Class) return Push_Callback;

   type Labeled_Button is abstract new Button with private;
   ------------------------------------------------
   -- procedure Set_Text
   --
   -- Set text for a labeled button
   ------------------------------------------------
   procedure Set_Text(
      Obj  : in out Labeled_Button;
      Text : in     String);

private
   type Button is abstract new Widget with record
      Push_Handler : Push_Callback := null;
   end record;

   type Labeled_Button is abstract new Button with null record;
end Mcc.Gui.Widget.Button;