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
package body Mcc.Gui.Colors is

   --------------
   -- To_Color --
   --------------

   function To_Color (RGB : in RGB_Color) return Color is
   begin
      return Color'(
         Red   => Intensity((RGB/256**2) mod 256),
         Green => Intensity((RGB/256)    mod 256),
         Blue  => Intensity( RGB         mod 256));
   end To_Color;

   function To_Color (RGB : in String) return Color is
   begin
      return To_Color(RGB_Color'Value("16#" & RGB & "#"));
   end To_Color;

end Mcc.Gui.Colors;