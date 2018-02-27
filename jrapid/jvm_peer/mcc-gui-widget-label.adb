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
with Java_Strings;
with Java.Awt.Container;
with Javax.Swing.JLabel;
with Javax.Swing.SwingConstants;
with Java.Lang.Object;
with Ada.Unchecked_Conversion;
with Peer.Container;
package body Mcc.Gui.Widget.Label is

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj     : in out Label;
      Parent  : in     Mcc.Gui.Container.Container'Class;
      X       : in     Integer;
      Y       : in     Integer;
      Width   : in     Natural;
      Height  : in     Natural;
      Justify : in     Justification;
      Text    : in     String)
   is
      Alignment : Java.Int;
      Label     : Javax.Swing.JLabel.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      
      case Justify is
         when Left =>
            Alignment := Javax.Swing.SwingConstants.LEFT;
         when Right =>
            Alignment := Javax.Swing.SwingConstants.RIGHT;
         when Center =>
            Alignment := Javax.Swing.SwingConstants.CENTER;
      end case;
      
      Label := Javax.Swing.JLabel.new_JLabel(
         Java_Strings.To_Java_String(Text),
         Alignment);
         
      Obj.My_Peer := Java.Lang.Object.Ref(Label);

      Javax.Swing.JLabel.setBounds(Label,x,y,Width,Height);
      
      Peer.Container.Add_To_Parent(Parent,Label);
      Obj.Parent := Convert(Parent'Unchecked_Access);
   end Create;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Obj  : in out Label;
      Text : in     String)
   is
   begin
      Javax.Swing.JLabel.setText(
         Javax.Swing.JLabel.Ref(Obj.My_Peer),
         Java_Strings.To_Java_String(Text));
   end Set_Text;

end Mcc.Gui.Widget.Label;

