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
-----------------------------------------------------------------------------
with Ada.Text_IO;
package Mcc.Gui.Colors is
   -- these constants are basic colors for the color type
   -- in Mcc.Gui.  

   type Color_Enum is (
      Red,Pink,
      Green,LightGreen,DarkGreen,ForestGreen,
      Blue,LightBlue,DarkBlue,
      Cyan,LightCyan,
      Purple,Magenta,
      Yellow,LightYellow,
      Orange,
      Gray,LightGray,DarkGray,
      Brown,
      Black,White);
   package Color_IO is new Ada.Text_IO.Enumeration_IO(Color_Enum);
   type Color_Array is array(Color_Enum'Range) of Color;
   
   Named_Color : constant Color_Array :=
      (Red         => Color'(255,  0,  0),
       Pink        => Color'(255,175,175),
       Green       => Color'(  0,255,  0),
       LightGreen  => Color'(128,255,128),
       DarkGreen   => Color'(  0,128,  0),
       ForestGreen => Color'(  0, 92,  0),
       Blue        => Color'(  0,  0,255),
       DarkBlue    => Color'(  0,  0,128),
       LightBlue   => Color'(128,128,255),
       Cyan        => Color'(  0,255,255),
       LightCyan   => Color'(128,255,255),
       Purple      => Color'(200,  0,200),
       Magenta     => Color'(255,  0,255),
       Yellow      => Color'(255,255,  0),
       LightYellow => Color'(255,255,128),
       Orange      => Color'(255,200,  0),
       Gray        => Color'(128,128,128),
       LightGray   => Color'(192,192,192),
       DarkGray    => Color'( 64, 64, 64),
       Brown       => Color'(128, 64,  0),
       Black       => Color'(  0,  0,  0),
       White       => Color'(255,255,255));
   ----------------------------------------------------
   -- function To_Color
   --
   -- takes an integer between 0 and 16#FFFFFF#, a
   -- value from the Color_Enum enumeration type
   -- or a string of the form RRGGBB and converts it 
   -- to a color
   ----------------------------------------------------
   subtype RGB_Color is Natural range 0..16#FFFFFF#;
   function To_Color (RGB : in RGB_Color) return Color;
   function To_Color (RGB : in String) return Color;
end Mcc.Gui.Colors;
