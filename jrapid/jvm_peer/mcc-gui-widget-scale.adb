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
with Javax.Swing.JSlider;
with Javax.Swing.SwingConstants;
with Peer.Listeners;
with Java.Awt.Container;
with Java.Awt.Component;
with Ada.Unchecked_Conversion;
with Java.Lang.Object;
with Peer.Container;
package body Mcc.Gui.Widget.Scale is

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj                  : in out Scale;
      Parent               : in     Mcc.Gui.Container.Container'Class;
      X                    : in     Integer;
      Y                    : in     Integer;
      Width                : in     Natural;
      Height               : in     Natural;
      Min                  : in     Integer;
      Max                  : in     Integer;
      Mark_Every           : in     Natural;
      Orientation          : in     Scale_Orientation := Horizontal;
      By                   : in     Natural := 1)
   is
      Scale  : Javax.Swing.JSlider.Ref;
      type Parent_Access is access constant Mcc.Gui.Container.Container'Class;
      function Convert is new Ada.Unchecked_Conversion(
         Parent_Access,Mcc.Gui.Container.Container_Pointer);
      Java_Orientation : Java.Int;
   begin
      Obj.Key_Handler := null;
      Obj.Mouse_Handler := null;
      Obj.Change_Listener := null;
      
      if Orientation = Horizontal then
         Java_Orientation := Javax.Swing.SwingConstants.HORIZONTAL;
      else
         Java_Orientation := Javax.Swing.SwingConstants.VERTICAL;
      end if;
      
      Scale := Javax.Swing.JSlider.new_JSlider(
         Java_Orientation,Min,Max,Min);
         
      Obj.My_Peer := Java.Lang.Object.Ref(Scale);

      Javax.Swing.JSlider.setBounds(Scale,x,y,Width,Height);
      Javax.Swing.JSlider.setMinorTickSpacing(
         Scale,
         By);
      Javax.Swing.JSlider.setMajorTickSpacing(
         Scale,
         Mark_Every);
      Javax.Swing.JSlider.setPaintTicks(
         Scale,
         true);
      Javax.Swing.JSlider.setPaintLabels(
         Scale,
         true);
         
      Peer.Container.Add_To_Parent(Parent,Scale);
      Obj.Parent := Convert(Parent'Unchecked_Access);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Obj : in Scale) return Integer is
   begin
      return Javax.Swing.JSlider.getValue(
         Javax.Swing.JSlider.Ref(Obj.My_Peer));
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Obj      : in out Scale;
      Location : in     Integer)
   is
   begin
      Javax.Swing.JSlider.setValue(
         Javax.Swing.JSlider.Ref(Obj.My_Peer),
         Location);
   end Set;

   procedure Dummy_Scale_Listener(
      Obj   : in out Scale'Class) is
   begin
      null;
   end Dummy_Scale_Listener;
   ------------------------
   -- Set_Scale_Listener --
   ------------------------

   procedure Set_Scale_Listener
     (Obj           : in Scale_Pointer;
      Listener      : in Scale_Listener)
   is
   begin
      if Obj.Change_Listener = null and then Listener /= null then
         Javax.Swing.JSlider.addChangeListener(
            Javax.Swing.JSlider.Ref(Obj.My_Peer),
            Peer.Listeners.new_Scale_Listener(Obj).I_ChangeListener);
         Obj.Change_Listener := Listener;
      elsif Obj.Change_Listener /= null and then Listener = null then
         Obj.Change_Listener := Dummy_Scale_Listener'access;
      else
         Obj.Change_Listener := Listener;
      end if;
   end Set_Scale_Listener;

   function Get_Scale_Listener(
      Obj : in Scale'Class) return Scale_Listener is
   begin
      if Obj.Change_Listener = Dummy_Scale_Listener'access then
         return null;
      else
         return Obj.Change_Listener;
      end if;
   end Get_Scale_Listener;

end Mcc.Gui.Widget.Scale;

