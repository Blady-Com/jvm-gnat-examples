-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC GUI PACKAGE LIBRARY - JVM IMPLEMENTATION
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
with Java.Awt.Font;
with Java.Awt.Component;
with Java.Lang.Object;
with Java.Lang.String;
with Java_Strings;
with Peer.Scrollpane;
package body Mcc.Gui.Fonts is
   type Family_Array is array(Font_Family) of Java.Lang.String.Ref;
   Family_Names : Family_Array := 
      (Serif      => Java_Strings.To_Java_String("Serif"),
       Sans_Serif => Java_Strings.To_Java_String("SansSerif"),
       Monospaced => Java_Strings.To_Java_String("Courier"));
   type Style_Array is array(Font_Style) of Java.Int;
   Style_Constants : Style_Array :=
      (Plain       => Java.Awt.Font.PLAIN,
       Bold        => Java.Awt.Font.BOLD,
       Italic      => Java.Awt.Font.ITALIC,
       Bold_Italic => Java.Awt.Font.BOLD + Java.Awt.Font.ITALIC);
   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Font;
      Family : in     Font_Family := Serif;
      Size   : in     Font_Size := 12;
      Style  : in     Font_Style := Plain)
   is
   begin
      Obj.My_Peer := Java.Lang.Object.Ref(
         Java.Awt.Font.new_Font(
            Family_Names(Family),
            Style_Constants(Style),
            Size));
   end Create;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Obj      : in out Sized_Object'Class;
      New_Font : in     Font)
   is
   begin
      if Obj.My_Peer.all in Peer.Scrollpane.ListScrollPane_Typ'Class then
         Java.Awt.Component.setFont(
            Java.Awt.Component.Ref(
               Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer).Listbox),
            Java.Awt.Font.Ref(New_Font.My_Peer));
      else
         Java.Awt.Component.setFont(
            Java.Awt.Component.Ref(Obj.My_Peer),
            Java.Awt.Font.Ref(New_Font.My_Peer));
      end if;
   end Set_Font;

end Mcc.Gui.Fonts;

