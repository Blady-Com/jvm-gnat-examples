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
package Mcc.Gui.Widget.Listbox is

   type Listbox is new Widget with null record;
   type Listbox_Pointer is access all Listbox'Class;
   
   ------------------------------------------------
   -- procedure Create
   --
   -- create a listbox at a location within a container
   ------------------------------------------------
   procedure Create(
      Obj                  : in out Listbox;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural;
      Horizontal_Scrollbar : in     Boolean;
      Vertical_Scrollbar   : in     Boolean);

   ------------------------------------------------
   -- procedure Add_Entry
   --
   -- add the text after the location entry
   -- use 0 to add to front
   ------------------------------------------------
   procedure Add_Entry(
      Obj      : in out Listbox;
      Location : in     Natural;
      Text     : in     String);

   ------------------------------------------------
   -- procedure Delete_Entry
   --
   -- Delete a range of entries
   ------------------------------------------------
   procedure Delete_Entry(
      Obj   : in out Listbox;
      Start : in     Natural;
      Stop  : in     Natural);

   ------------------------------------------------
   -- procedure Clear
   --
   -- Clear listbox
   ------------------------------------------------
   procedure Clear(Obj  : in out Listbox);

   ------------------------------------------------
   -- function Count
   --
   -- get number of items in Listbox
   ------------------------------------------------
   function Count(Obj : in Listbox) return Natural;

   ------------------------------------------------
   -- procedure Select_Item
   --
   -- Select number item in Listbox
   -- Use 0 to select nothing.
   ------------------------------------------------
   procedure Select_Item(
      Obj    : in out Listbox;
      Number : in     Natural);

   ------------------------------------------------
   -- function Get_Selected
   --
   -- return number of selected item, or 0 if
   -- nothing selected
   ------------------------------------------------
   function Get_Selected(Obj : in Listbox) return Natural;

end Mcc.Gui.Widget.Listbox;