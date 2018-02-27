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
package Mcc.Gui.Widget.Text_Entry is

   type Text_Entry is new Widget with null record;
   type Text_Entry_Pointer is access all Text_Entry'Class;

   ------------------------------------------------
   -- procedure Create
   --
   -- create a text entry at a location within a container
   ------------------------------------------------
   procedure Create(
      Obj    : in out Text_Entry;
      Parent : in     Mcc.Gui.Container.Container'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Width  : in     Natural;
      Height : in     Natural);

   ------------------------------------------------
   -- procedure Set_Text
   --
   -- Set text for a text entry
   ------------------------------------------------
   procedure Set_Text(
      Obj  : in out Text_Entry;
      Text : in     String);

   ------------------------------------------------
   -- procedure Set_Text
   --
   -- Set text for a text entry using
   -- image of integer (w/o leading spaces)
   ------------------------------------------------
   procedure Set_Text(
      Obj  : in out Text_Entry;
      Text : in     Integer);
      
   ------------------------------------------------
   -- procedure Clear
   --
   -- Clear text entry
   ------------------------------------------------
   procedure Clear(Obj  : in out Text_Entry);

   ------------------------------------------------
   -- function Get_Text_Length
   --
   -- get length of text currently in entry
   ------------------------------------------------
   function Get_Text_Length(Obj : in Text_Entry) return Natural;

   ------------------------------------------------
   -- procedure Highlight
   --
   -- Highlight text entry from start to stop
   -- start and end are positions between characters
   -- 0 is before the first character.  If start=stop
   -- then this procedure moves the cursor
   ------------------------------------------------
   procedure Highlight(Obj : in out Text_Entry);
   procedure Highlight(
      Obj   : in out Text_Entry;
      Start : in     Natural;
      Stop  : in     Natural);

   ------------------------------------------------
   -- function(s) Get_Text 
   --
   -- return contents of text entry as integer, float 
   -- or string
   ------------------------------------------------
   function Get_Text(Obj : in Text_Entry) return String;
   function Get_Text(Obj : in Text_Entry) return Integer;
   function Get_Text(Obj : in Text_Entry) return Float;

end Mcc.Gui.Widget.Text_Entry;