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
package Mcc.Gui.Widget.Dropdown is

   type Dropdown is new Widget with null record;
   type Dropdown_Pointer is access all Dropdown'Class;
   
   ------------------------------------------------
   -- procedure Create
   --
   -- create a Dropdown at a location within a container
   -- dimensions are for when the list is closed
   -- Number_Rows is the number of rows when list is open
   ------------------------------------------------
   procedure Create(
      Obj                  : in out Dropdown;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural;
      Number_Rows          : in     Natural);

   ------------------------------------------------
   -- procedure Add_Entry
   --
   -- add the text after the location entry
   -- use 0 to add to front
   ------------------------------------------------
   procedure Add_Entry(
      Obj      : in out Dropdown;
      Location : in     Natural;
      Text     : in     String);

   ------------------------------------------------
   -- procedure Delete_Entry
   --
   -- Delete a range of entries
   ------------------------------------------------
   procedure Delete_Entry(
      Obj   : in out Dropdown;
      Start : in     Natural;
      Stop  : in     Natural);

   ------------------------------------------------
   -- procedure Clear
   --
   -- Clear Dropdown
   ------------------------------------------------
   procedure Clear(Obj  : in out Dropdown);

   ------------------------------------------------
   -- function Count
   --
   -- get number of items in Dropdown
   ------------------------------------------------
   function Count(Obj : in Dropdown) return Natural;

   ------------------------------------------------
   -- procedure Select_Item
   --
   -- Select number item in Dropdown
   -- Use 0 to select nothing.
   ------------------------------------------------
   procedure Select_Item(
      Obj    : in out Dropdown;
      Number : in     Natural);

   ------------------------------------------------
   -- function Get_Selected
   --
   -- return number of selected item, or 0 if
   -- nothing selected
   ------------------------------------------------
   function Get_Selected(Obj : in Dropdown) return Natural;

end Mcc.Gui.Widget.Dropdown;