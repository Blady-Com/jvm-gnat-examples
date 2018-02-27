-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC GUI PACKAGE LIBRARY                               
--           Copyright (C) 1999 Martin C. Carlisle.       
--
-- Adapted from RAPID version 1.2 suggestion by W. Blair Watkinson II
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
package Mcc.Gui.Widget.Scale is

   type Scale_Orientation is (Horizontal, Vertical);
   
   type Scale is new Widget with private;
   type Scale_Pointer is access all Scale'Class;
   
   ------------------------------------------------
   -- procedure Create
   --
   -- create a scale slider at a location within a container
   --
   -- can take on a range of values from Min to Max
   -- each value must be a multiple of by
   -- Hash marks are displayed every Mark_Every
   ------------------------------------------------
   procedure Create(
      Obj                  : in out Scale;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural;
      Min                  : in     Integer;
      Max                  : in     Integer;
      Mark_Every           : in     Natural;
      Orientation          : in     Scale_Orientation := Horizontal;
      By                   : in     Natural := 1);

   ------------------------------------------------
   -- procedure Set
   --
   -- change the location of the scale widget
   ------------------------------------------------
   procedure Set(
      Obj      : in out Scale;
      Location : in     Integer);

   ------------------------------------------------
   -- Function Get
   --
   -- return the location of the scale widget
   ------------------------------------------------
   function Get(Obj : in Scale) return Integer;

   ------------------------------------------------
   -- procedure Set_Change_Listener
   --
   -- Set callback for when scale value is changed
   ------------------------------------------------
   type Scale_Listener is access procedure (Obj : in out Scale'Class);
   procedure Set_Scale_Listener(
      Obj           : in Scale_Pointer;
      Listener      : in Scale_Listener);
   
   function Get_Scale_Listener(
      Obj : in Scale'Class) return Scale_Listener;
private
   type Scale is new widget with record
      Change_Listener : Scale_Listener;
   end record;
end Mcc.Gui.Widget.Scale;