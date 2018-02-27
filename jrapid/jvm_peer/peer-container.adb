-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           JVM IMPLEMENTATION                               
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
with Mcc.Gui.Container.Frame;
with Mcc.Gui.Container.Window;
with Javax.Swing.RepaintManager;
package body peer.container is
   -- since we need to call GetContentPane for JFrame, but
   -- not for JPanel, do a check on the container type here
   -- so we don't need to replicate across everything
   procedure Add_To_Parent(
         Parent    : in     Mcc.Gui.Container.Container'Class;
         Component : access Java.Awt.Component.Typ'Class) is
      Ignore    : Java.Awt.Component.Ref;
   begin
      if Parent in Mcc.Gui.Container.Window.Window'Class then
         Ignore := Java.Awt.Container.add(
            Javax.Swing.JFrame.getContentPane(
               Javax.Swing.JFrame.Ref(
                  Mcc.Gui.Container.Get_Peer(Parent))),
            Component);
         Java.Awt.Component.repaint(
            Component);
         Javax.Swing.RepaintManager.validateInvalidComponents(
            Javax.Swing.RepaintManager.currentManager(
               Component));
      else
         Ignore := Java.Awt.Container.add(
            Java.Awt.Container.Ref(
               Mcc.Gui.Container.Get_Peer(Parent)),
            Component);
         Java.Awt.Component.repaint(
            Component);
         Javax.Swing.RepaintManager.validateInvalidComponents(
            Javax.Swing.RepaintManager.currentManager(
               Component));
      end if;
   end Add_To_Parent;
end peer.container;