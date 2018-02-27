-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC GUI PACKAGE LIBRARY                               
--           Copyright (C) 1999 Martin C. Carlisle.       
--
-- Adapted from RAPID version 1.2 suggestion by Dmitri Anisimkov         
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
package Mcc.Gui.Widget.Progress is

   type Progress is new Widget with null record;
   type Progress_Pointer is access all Progress'Class;
   
   subtype Progress_Amount is Float range 0.0..1.0;

   ------------------------------------------------
   -- procedure Create
   --
   -- create a progress bar at a location within a container
   ------------------------------------------------
   procedure Create(
      Obj                  : in out Progress;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural);

   ------------------------------------------------
   -- procedure Set_Progress
   --
   -- change the percentage of progress 
   -- must be a real value in [0.0,1.0]
   ------------------------------------------------
   procedure Set_Progress(
      Obj      : in out Progress;
      Amount   : in     Progress_Amount);
  
end Mcc.Gui.Widget.Progress;