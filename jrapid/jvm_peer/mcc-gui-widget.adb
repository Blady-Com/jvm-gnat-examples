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
with Javax.Swing.JComponent;
with Java.Awt.Component;
with Java.Awt.Color;
with Java_Strings;
with Peer.Scrollpane;
package body Mcc.Gui.Widget is

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (Obj : in Widget'Class)
      return Mcc.Gui.Container.Container_Pointer
   is
   begin
      return Obj.Parent;
   end Get_Parent;

   --------------------------
   -- Set_Foreground_Color --
   --------------------------

   procedure Set_Foreground_Color
     (Obj       : in out Widget;
      New_Color : in     Color)
   is
   begin
      if Obj.My_Peer.all in Peer.Scrollpane.ListScrollPane_Typ'Class then
         Java.Awt.Component.setForeground(
            Java.Awt.Component.Ref(
               Peer.ScrollPane.ListScrollPane_Ref(Obj.My_Peer).Listbox),
            Java.Awt.Color.new_Color(
               Java.Int(New_Color.Red),
               Java.Int(New_Color.Green),
               Java.Int(New_Color.Blue)));
      else
         Java.Awt.Component.setForeground(
            Java.Awt.Component.Ref(Obj.My_Peer),
            Java.Awt.Color.new_Color(
               Java.Int(New_Color.Red),
               Java.Int(New_Color.Green),
               Java.Int(New_Color.Blue)));
     end if;
   end Set_Foreground_Color;

   ----------------------
   -- Set_Tooltip_Text --
   ----------------------

   procedure Set_Tooltip_Text
     (Obj  : in out Widget;
      Text : in String)
   is
   begin
      Javax.Swing.JComponent.setToolTipText(
         Javax.Swing.JComponent.Ref(Obj.My_Peer),
         Java_Strings.To_Java_String(Text));
   end Set_Tooltip_Text;

end Mcc.Gui.Widget;

