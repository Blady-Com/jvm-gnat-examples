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
with Java.Awt.Color;
with Javax.Swing.Border.Border;
with Javax.Swing.BorderFactory;
with Javax.Swing.JPanel;
with Java.Lang.Object;
with Java.Awt.Layoutmanager;
with Peer.Container;
package body Mcc.Gui.Container.Frame is

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj          : in out Frame;
      Parent       : in     Container'Class;
      X            : in     Integer;
      Y            : in     Integer;
      Width        : in     Natural;
      Height       : in     Natural;
      Border_Width : in     Natural := 1)
   is
      Border : Javax.Swing.Border.Border.Ref;
      Panel  : Javax.Swing.JPanel.Ref;
   begin
      if Border_Width = 0 then
         Border := Javax.Swing.Border.Border.Ref(Javax.Swing.BorderFactory.createEmptyBorder);
      else
         Border := Javax.Swing.Border.Border.Ref(Javax.Swing.BorderFactory.createLineBorder(
            Java.Awt.Color.black,
            Border_Width));
      end if;
      
      -- no layout manager
      declare
         Null_Layout : Java.Awt.Layoutmanager.Ref := null;
      begin
         Panel := Javax.Swing.JPanel.new_JPanel(Null_Layout);
      end;
      
      Obj.My_Peer := Java.Lang.Object.Ref(Panel);

      Javax.Swing.JPanel.setBorder(Panel,Border);
      Javax.Swing.JPanel.setBounds(Panel,x,y,Width,Height);
      
      Peer.Container.Add_To_Parent(Parent,Panel);
      Javax.Swing.JPanel.repaint(Panel);
   end Create;

end Mcc.Gui.Container.Frame;

