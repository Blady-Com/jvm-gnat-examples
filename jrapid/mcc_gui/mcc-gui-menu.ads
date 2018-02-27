-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC GUI PACKAGE LIBRARY                               
--           Copyright (C) 1999 Martin C. Carlisle.                
--                                                                 
-- RAPID is free software; you can redistribute it and/or modify
-- it under terms of the GNU General Public License as published  
-- by the Free Software Foundation; either version 2, or (at your
-- option) any later version.  RAPID is distributed in the hope that 
-- it will be useful, but WITHOUT ANY WARRANTY; without even the 
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU General Public License for more details.  
-- You should have received a copy of the GNU General Public License
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
package Mcc.Gui.Menu is
   type Menu_Callback is access procedure;

   type Menu_Item is abstract new Object with private;
   -------------------------------------------------------
   -- procedure Delete
   --
   -- Delete a menu or choice
   -------------------------------------------------------
   procedure Delete(Obj : in out Menu_Item);

   type Menu is abstract new Menu_Item with private;

   type Window_Menu is new Menu with private;
   -------------------------------------------------------
   -- procedure Create
   --
   -- Create a menu along the top of a container
   -------------------------------------------------------
   procedure Create(
      Obj    : in out Window_Menu;
--      Window : in     Mcc.Gui.Container.Container'Class);
      Window : in     Mcc.Gui.Object'Class);

   type Submenu is new Menu with private;
   -------------------------------------------------------
   -- procedure Add_Submenu
   --
   -- Create a submenu (at end) or after Location item
   -- (0 is at beginning)
   -------------------------------------------------------

   procedure Add_Submenu
     (Obj         : in out Submenu;
      Text        : in     String;
      Underline   : in     Natural;
      Parent_Menu : in     Menu'Class;
      On_Post     : in     Menu_Callback := null);
      
   procedure Add_Submenu
     (Obj         : in out Submenu;
      Text        : in     String;
      Underline   : in     Natural;
      Parent_Menu : in     Menu'Class;
      Location    : in     Natural;
      On_Post     : in     Menu_Callback := null);

   type Choice is new Menu_Item with private;
   -------------------------------------------------------
   -- procedure Add_Choice
   --
   -- Add a menu choice to a menu
   -- underline is # of character for shortcut (0 if none)
   -- at end or after Location (0 for beginning)
   -------------------------------------------------------
   procedure Add_Choice(
      Obj         : in out Choice;
      To_Menu     : in     Menu'Class;
      Text        : in     String;
      Action      : in     Menu_Callback;
      Underline   : in     Natural;
      Accelerator : in     String := "");

   procedure Add_Choice(
      Obj         : in out Choice;
      To_Menu     : in     Menu'Class;
      Text        : in     String;
      Action      : in     Menu_Callback;
      Underline   : in     Natural;
      Location    : in     Natural;
      Accelerator : in     String := "");

   -------------------------------------------------------
   -- procedure Enable
   -------------------------------------------------------
   procedure Enable(Obj : in out Choice);

   -------------------------------------------------------
   -- procedure Disable
   -------------------------------------------------------
   procedure Disable(Obj : in out Choice);
   
   -------------------------------------------------------
   -- Menu separators can not be selected, they 
   -- merely serve to separate a menu into blocks of choices
   --
   -- At this time, calling Delete on a separator has no effect
   -------------------------------------------------------
   type Separator is new Menu_Item with private;
   
   procedure Add_Separator(
      Obj         : in out Separator;
      To_Menu     : in     Menu'Class);
      
   procedure Add_Separator(
      Obj         : in out Separator;
      To_Menu     : in     Menu'Class;
      Location    : in     Natural);
private
   type String_Pointer is access all String;
   type Menu_Pointer is access all Menu'Class;

   type Menu_Item is abstract new Object with record
      Parent : Menu_Pointer;
      Text   : String_Pointer;
   end record;

   type Menu is abstract new Menu_Item with null record;

   type Window_Menu is new Menu with null record;

   type Submenu is new Menu with null record;

   type Choice is new Menu_item with null record;
   
   type Separator is new Menu_Item with null record;
end Mcc.Gui.Menu;