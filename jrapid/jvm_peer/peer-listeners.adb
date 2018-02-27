-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           JVM PEER FOR THE MCC GUI PACKAGE LIBRARY 
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
with Mcc.Gui.Container.Window;
use type Mcc.Gui.Container.Window.Close_Handler;
use type Mcc.Gui.Container.Window.Resize_Handler;
with Mcc.Gui.Menu;
use type Mcc.Gui.Menu.Menu_Callback;
with Java.Lang.System;
with Java.Awt.Event.ActionListener;
with Java.Awt.Event.InputEvent;
with Java.Awt.Event.WindowEvent;
with Java.Awt.Component;
with Java.Awt.Dimension;
package body Peer.Listeners is
   
   --------------------------------------------------------------------
   -- Window listening routines
   --------------------------------------------------------------------
   function New_Window_Adapter (
         Mcc_Object : Mcc.Gui.Container.Window.Window_Pointer;        
         This       : Window_Adapter_Ref                      := null ) 
     return Window_Adapter_Ref is 
      Super : Java.Awt.Event.WindowAdapter.Ref := 
         Java.Awt.Event.WindowAdapter.new_Windowadapter(
            Java.Awt.Event.WindowAdapter.Ref(This));
   begin
      This.Mcc_Object := Mcc_Object;
      return This;
   end New_Window_Adapter;
   
   procedure windowClosing ( This : access Window_Adapter_Typ;
      P1 : access java.awt.event.WindowEvent.Typ'Class) is
      Handler : Mcc.Gui.Container.Window.Close_Handler;
   begin
      Handler := Mcc.Gui.Container.Window.Get_Close_Handler(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(This.Mcc_Object.all);
      end if;
   end windowClosing;

   procedure windowClosed ( This : access Window_Adapter_Typ;
      P1 : access java.awt.event.WindowEvent.Typ'Class) is
   begin
      if This.Mcc_Object.all in Mcc.Gui.Container.Window.Main_Window'Class then
         Java.Lang.System.Exit_K(0);
      end if;
   end windowClosed;

   --------------------------------------------------------------------
   -- Window resize listening routines
   --------------------------------------------------------------------
   function New_Component_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Component_Adapter_Ref                      := null ) 
     return Component_Adapter_Ref is 
      Super : Java.Awt.Event.ComponentAdapter.Ref := 
         Java.Awt.Event.ComponentAdapter.new_Componentadapter(
            Java.Awt.Event.ComponentAdapter.Ref(This));
   begin
      This.Mcc_Object := Mcc_Object;
      return This;
   end New_Component_Adapter;

   procedure componentResized (This : access Component_Adapter_Typ;
      P1 : access java.awt.event.ComponentEvent.Typ'Class) is
      Handler : Mcc.Gui.Container.Window.Resize_Handler;
      Component : Java.Awt.Component.Ref;
      Dimension : Java.Awt.Dimension.Ref;
   begin
      Component := Java.Awt.Event.ComponentEvent.GetComponent(P1);
      Dimension := Java.Awt.Component.getSize(Component);
      Handler := Mcc.Gui.Container.Window.Get_Resize_Handler(
         Mcc.Gui.Container.Window.Window'Class(This.Mcc_Object.all));
      if Handler /= null then
         Handler.all(
            Obj    => Mcc.Gui.Container.Window.Window'Class(
               This.Mcc_Object.all),
            Width  => Dimension.Width,
            Height => Dimension.Height);
      end if;
   end componentResized;

   --------------------------------------------------------------------
   -- Keyboard listening routines
   --------------------------------------------------------------------
   function New_Key_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Key_Adapter_Ref                      := null ) 
     return Key_Adapter_Ref is 
      Super : Java.Awt.Event.KeyAdapter.Ref := 
         Java.Awt.Event.KeyAdapter.new_Keyadapter(
            Java.Awt.Event.KeyAdapter.Ref(This));
   begin
      This.Mcc_Object := Mcc_Object;
      return This;
   end New_Key_Adapter;

   procedure keyPressed (
      This : access Key_Adapter_Typ; 
      P1   : access java.awt.event.KeyEvent.Typ'Class) is
      Handler : Mcc.Gui.Key_Listener;
      use type Mcc.Gui.Key_Listener;
   begin
      Handler := Mcc.Gui.Get_Key_Listener(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(
            Obj   => This.Mcc_Object.all,
            Event => Mcc.Gui.Key_Event'(
               Key    => Mcc.Gui.Key_Type(
                  Java.Awt.Event.KeyEvent.getKeyCode(P1)),
               Action => Mcc.Gui.Press));
      end if;
   end keyPressed;
   
   procedure keyReleased (
      This : access Key_Adapter_Typ; 
      P1   : access java.awt.event.KeyEvent.Typ'Class) is
      Handler : Mcc.Gui.Key_Listener;
      use type Mcc.Gui.Key_Listener;
   begin
      Handler := Mcc.Gui.Get_Key_Listener(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(
            Obj   => This.Mcc_Object.all,
            Event => Mcc.Gui.Key_Event'(
               Key    => Mcc.Gui.Key_Type(
                  Java.Awt.Event.KeyEvent.getKeyCode(P1)),
               Action => Mcc.Gui.Release));
      end if;
   end keyReleased;
   
   --------------------------------------------------------------------
   -- Mouse listening routines
   --------------------------------------------------------------------
   function New_Mouse_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Mouse_Adapter_Ref                      := null ) 
     return Mouse_Adapter_Ref is 
      Super : Java.Awt.Event.MouseAdapter.Ref := 
         Java.Awt.Event.MouseAdapter.new_Mouseadapter(
            Java.Awt.Event.MouseAdapter.Ref(This));
   begin
      This.Mcc_Object := Mcc_Object;
      return This;
   end New_Mouse_Adapter;

   function Get_Button(
      P1 : access Java.Awt.Event.MouseEvent.Typ'Class) 
      return Mcc.Gui.Mouse_Button is
      Button_Number : Integer;
   begin
      Button_Number := Integer(Java.Awt.Event.MouseEvent.getModifiers(P1));
      if Button_Number/Integer(Java.Awt.Event.InputEvent.BUTTON1_MASK) 
         mod 2 = 1 then
         return Mcc.Gui.Left;
      elsif Button_Number/Integer(Java.Awt.Event.InputEvent.BUTTON2_MASK) 
         mod 2 = 1 then
         return Mcc.Gui.Middle;
      elsif Button_Number/Integer(Java.Awt.Event.InputEvent.BUTTON3_MASK) 
         mod 2 = 1 then
         return Mcc.Gui.Right;
      else
         return Mcc.Gui.None;
      end if;
   end Get_Button;
   
   procedure MousePressed (
      This : access Mouse_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class) is
      Handler : Mcc.Gui.Mouse_Listener;
      use type Mcc.Gui.Mouse_Listener;
      function Get_Press_Action(
         P1 : access Java.Awt.Event.MouseEvent.Typ'Class) 
            return Mcc.Gui.Mouse_Action_Type is
      begin
         if Java.Awt.Event.MouseEvent.getClickCount(P1) >= 2 then
            return Mcc.Gui.Double_Click;
         else
            return Mcc.Gui.Press;
         end if;
      end Get_Press_Action;
   begin
      Handler := Mcc.Gui.Get_Mouse_Listener(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(
            Obj   => This.Mcc_Object.all,
            Event => Mcc.Gui.Mouse_Event'(
               Button => Get_Button(P1),
               Action => Get_Press_Action(P1),
               X      => Java.Awt.Event.MouseEvent.getX(P1),
               Y      => Java.Awt.Event.MouseEvent.getY(P1)));
      end if;
   end MousePressed;
   
   procedure MouseReleased (
      This : access Mouse_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class) is
      Handler : Mcc.Gui.Mouse_Listener;
      use type Mcc.Gui.Mouse_Listener;
   begin
      Handler := Mcc.Gui.Get_Mouse_Listener(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(
            Obj   => This.Mcc_Object.all,
            Event => Mcc.Gui.Mouse_Event'(
               Button => Get_Button(P1),
               X      => Java.Awt.Event.MouseEvent.getX(P1),
               Y      => Java.Awt.Event.MouseEvent.getY(P1),
               Action => Mcc.Gui.Release));
      end if;
   end MouseReleased;

   function New_Mouse_Motion_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Mouse_Motion_Adapter_Ref           := null ) 
     return Mouse_Motion_Adapter_Ref is 
      Super : Java.Awt.Event.MouseMotionAdapter.Ref := 
         Java.Awt.Event.MouseMotionAdapter.new_MouseMotionadapter(
            Java.Awt.Event.MouseMotionAdapter.Ref(This));
   begin
      This.Mcc_Object := Mcc_Object;
      return This;
   end New_Mouse_Motion_Adapter;

   procedure MouseMoved (
      This : access Mouse_Motion_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class) is
      Handler : Mcc.Gui.Mouse_Listener;
      use type Mcc.Gui.Mouse_Listener;
   begin
      Handler := Mcc.Gui.Get_Mouse_Listener(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(
            Obj   => This.Mcc_Object.all,
            Event => Mcc.Gui.Mouse_Event'(
               Button => Get_Button(P1),
               X      => Java.Awt.Event.MouseEvent.getX(P1),
               Y      => Java.Awt.Event.MouseEvent.getY(P1),
               Action => Mcc.Gui.Move));
      end if;
   end MouseMoved;
   
   procedure MouseDragged (
      This : access Mouse_Motion_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class) is
   begin
      MouseMoved(This,P1);
   end MouseDragged;

   --------------------------------------------------------------------
   -- Button listening routines
   --------------------------------------------------------------------
   function New_Button_Action_Listener (
         Mcc_Object : Mcc.Gui.Widget.Button.Button_Pointer;        
         This       : Button_Action_Listener_Ref             := null ) 
     return Button_Action_Listener_Ref is
      Super : Java.Lang.Object.Ref := 
         Java.Lang.Object.new_Object(
            Java.Lang.Object.Ref(This));
   begin
      This.Mcc_Object := Mcc_Object;
      return This;
   end New_Button_Action_Listener;

   procedure actionPerformed (
      This : access Button_Action_Listener_Typ;
      P1   : access Java.Awt.Event.ActionEvent.Typ'Class) is
      Handler : Mcc.Gui.Widget.Button.Push_Callback;
      use type Mcc.Gui.Widget.Button.Push_Callback;
   begin
      Handler := Mcc.Gui.Widget.Button.Get_Push_Callback(
         This.Mcc_Object.all);
      if Handler /= null then
         Handler.all(
            Obj   => This.Mcc_Object.all);
      end if;
   end actionPerformed;

   --------------------------------------------------------------------
   -- Menu listening routines
   --------------------------------------------------------------------
   function New_Menu_Action_Listener (
         Callback : Mcc.Gui.Menu.Menu_Callback;        
         This     : Menu_Action_Listener_Ref             := null ) 
     return Menu_Action_Listener_Ref is
      Super : Java.Lang.Object.Ref := 
         Java.Lang.Object.new_Object(
            Java.Lang.Object.Ref(This));
   begin
      This.Callback := Callback;
      return This;
   end New_Menu_Action_Listener;

   procedure actionPerformed (
      This : access Menu_Action_Listener_Typ;
      P1   : access Java.Awt.Event.ActionEvent.Typ'Class) is
   begin
      if This.Callback /= null then
         This.Callback.all;
      end if;
   end actionPerformed;

   function New_Menu_Listener (
         Callback : Mcc.Gui.Menu.Menu_Callback;        
         This     : Menu_Listener_Ref             := null ) 
     return Menu_Listener_Ref is
      Super : Java.Lang.Object.Ref := 
         Java.Lang.Object.new_Object(
            Java.Lang.Object.Ref(This));
   begin
      This.Callback := Callback;
      return This;
   end New_Menu_Listener;

   procedure menuSelected (
      This : access Menu_Listener_Typ;
      P1   : access Javax.Swing.Event.MenuEvent.Typ'Class) is
   begin
      if This.Callback /= null then
         This.Callback.all;
      end if;
   end menuSelected;

   procedure menuDeselected (
      This : access Menu_Listener_Typ;
      P1   : access Javax.Swing.Event.MenuEvent.Typ'Class) is
   begin
      null;
   end menuDeselected;
   
   procedure menuCanceled (
      This : access Menu_Listener_Typ;
      P1   : access Javax.Swing.Event.MenuEvent.Typ'Class) is
   begin
      null;
   end menuCanceled;
   
   --------------------------------------------------------------------
   -- Scale listening routines
   --------------------------------------------------------------------
   function New_Scale_Listener (
         Mcc_Object : Mcc.Gui.Widget.Scale.Scale_Pointer;        
         This       : Scale_Listener_Ref             := null ) 
     return Scale_Listener_Ref is
      Super : Java.Lang.Object.Ref := 
         Java.Lang.Object.new_Object(
            Java.Lang.Object.Ref(This));
   begin
      This.Scale := Mcc_Object;
      return This;
   end New_Scale_Listener;

   procedure stateChanged (
      This : access Scale_Listener_Typ;
      P1   : access Javax.Swing.Event.ChangeEvent.Typ'Class) is
      use type Mcc.Gui.Widget.Scale.Scale_Listener;
   begin
      if Mcc.Gui.Widget.Scale.Get_Scale_Listener(This.Scale.all) 
         /= null then
         Mcc.Gui.Widget.Scale.Get_Scale_Listener(This.Scale.all).all(
            This.Scale.all);
      end if;
   end stateChanged;
end Peer.Listeners;
