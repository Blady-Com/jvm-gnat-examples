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
with Mcc.Gui.Widget.Button;
with Mcc.Gui.Widget.Scale;
with Mcc.Gui.Menu;
with Java.Awt.Event.WindowAdapter;
with Java.Awt.Event.WindowEvent;
with Java.Awt.Event.ComponentAdapter;
with Java.Awt.Event.ComponentEvent;
with Java.Awt.Event.KeyAdapter;
with Java.Awt.Event.KeyEvent;
with Java.Awt.Event.MouseAdapter;
with Java.Awt.Event.MouseEvent;
with Java.Awt.Event.MouseMotionAdapter;
with Java.Awt.Event.ActionListener;
with Java.Awt.Event.ActionEvent;
with Java.Util.EventListener;
with Javax.Swing.Event.ChangeListener;
with Javax.Swing.Event.ChangeEvent;
with Javax.Swing.Event.MenuListener;
with Javax.Swing.Event.MenuEvent;

package Peer.Listeners is
   -----------------------------------------------------------
   -- use Window Adapter to respond to Window closing events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Window_Adapter_Typ is new Java.Awt.Event.WindowAdapter.Typ with 
      record 
         Mcc_Object : Mcc.Gui.Container.Window.Window_Pointer;  
      end record; 

   type Window_Adapter_Ref is access all Window_Adapter_Typ'Class; 

   -- New_Window_Adapter constructs a new adapter
   -- by simply calling the parent constructor, and 
   -- then filling in the Mcc_Object field
   function New_Window_Adapter (
         Mcc_Object : Mcc.Gui.Container.Window.Window_Pointer;        
         This       : Window_Adapter_Ref                      := null ) 
     return Window_Adapter_Ref; 

   -- this procedure simply calls the appropriate callback
   -- referenced in the Mcc_Object
   procedure windowClosing ( This : access Window_Adapter_Typ;
      P1 : access java.awt.event.WindowEvent.Typ'Class);

   -- this procedure calls system.exit if it is the main window
   -- o/w does nothing
   procedure windowClosed (This : access Window_Adapter_Typ;
      P1 : access java.awt.event.WindowEvent.Typ'Class);

   -----------------------------------------------------------
   -- use ComponentAdapter to respond to resizing events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Component_Adapter_Typ is new Java.Awt.Event.ComponentAdapter.Typ with 
      record 
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;  
      end record; 

   type Component_Adapter_Ref is access all Component_Adapter_Typ'Class; 
   
   function New_Component_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Component_Adapter_Ref                      := null ) 
     return Component_Adapter_Ref; 

   -- this procedure simply calls the appropriate callback
   -- referenced in the Mcc_Object
   procedure componentResized (This : access Component_Adapter_Typ;
      P1 : access java.awt.event.ComponentEvent.Typ'Class);

   -----------------------------------------------------------
   -- use KeyAdapter to respond to keyboard events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Key_Adapter_Typ is new Java.Awt.Event.KeyAdapter.Typ with 
      record 
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;  
      end record; 

   type Key_Adapter_Ref is access all Key_Adapter_Typ'Class; 
   
   function New_Key_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Key_Adapter_Ref                      := null ) 
     return Key_Adapter_Ref; 

   -- these procedures call the appropriate callback
   -- referenced in the Mcc_Object after adapting the
   -- arguments of the event
   procedure keyPressed (
      This : access Key_Adapter_Typ; 
      P1   : access java.awt.event.KeyEvent.Typ'Class);
   
   procedure keyReleased (
      This : access Key_Adapter_Typ; 
      P1   : access java.awt.event.KeyEvent.Typ'Class);
   
   -----------------------------------------------------------
   -- use MouseAdapter to respond to mouse events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Mouse_Adapter_Typ is new Java.Awt.Event.MouseAdapter.Typ with 
      record 
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;  
      end record; 

   type Mouse_Adapter_Ref is access all Mouse_Adapter_Typ'Class; 
   
   function New_Mouse_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Mouse_Adapter_Ref                      := null ) 
     return Mouse_Adapter_Ref; 

   -- these procedures call the appropriate callback
   -- referenced in the Mcc_Object after adapting the
   -- arguments of the event
   procedure mousePressed (
      This : access Mouse_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class);
   
   procedure mouseReleased (
      This : access Mouse_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class);
   
   type Mouse_Motion_Adapter_Typ is new 
      Java.Awt.Event.MouseMotionAdapter.Typ with 
      record 
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;  
      end record; 

   type Mouse_Motion_Adapter_Ref is access all 
      Mouse_Motion_Adapter_Typ'Class; 
   
   function New_Mouse_Motion_Adapter (
         Mcc_Object : Mcc.Gui.Sized_Object_Pointer;        
         This       : Mouse_Motion_Adapter_Ref            := null ) 
     return Mouse_Motion_Adapter_Ref; 

   -- these procedures call the appropriate callback
   -- referenced in the Mcc_Object after adapting the
   -- arguments of the event
   procedure mouseDragged (
      This : access Mouse_Motion_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class);
   
   procedure mouseMoved (
      This : access Mouse_Motion_Adapter_Typ; 
      P1   : access java.awt.event.MouseEvent.Typ'Class);

   -----------------------------------------------------------
   -- use Button_Action_Listener to respond to button push events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Button_Action_Listener_Typ( 
      I_ActionListener : access Java.Awt.Event.ActionListener.Typ)
      is new Java.Lang.Object.Typ with 
      record 
         Mcc_Object : Mcc.Gui.Widget.Button.Button_Pointer;  
      end record; 

   type Button_Action_Listener_Ref is access all Button_Action_Listener_Typ'Class; 
   
   function New_Button_Action_Listener (
         Mcc_Object : Mcc.Gui.Widget.Button.Button_Pointer;        
         This       : Button_Action_Listener_Ref             := null ) 
     return Button_Action_Listener_Ref; 

   procedure actionPerformed (
      This : access Button_Action_Listener_Typ;
      P1   : access Java.Awt.Event.ActionEvent.Typ'Class);
   
   -----------------------------------------------------------
   -- use Menu_Action_Listener to respond to menu events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Menu_Action_Listener_Typ(
      I_ActionListener : access Java.Awt.Event.ActionListener.Typ)
      is new Java.Lang.Object.Typ with 
      record 
         Callback : Mcc.Gui.Menu.Menu_Callback;  
      end record; 

   type Menu_Action_Listener_Ref is access all Menu_Action_Listener_Typ'Class; 
   
   function New_Menu_Action_Listener (
         Callback : Mcc.Gui.Menu.Menu_Callback;        
         This     : Menu_Action_Listener_Ref             := null ) 
     return Menu_Action_Listener_Ref; 

   procedure actionPerformed (
      This : access Menu_Action_Listener_Typ;
      P1   : access Java.Awt.Event.ActionEvent.Typ'Class);

   type Menu_Listener_Typ(
      I_EventListener  : access Java.Util.EventListener.Typ;
      I_MenuListener   : access Javax.Swing.Event.MenuListener.Typ)
      is new Java.Lang.Object.Typ with 
      record 
         Callback : Mcc.Gui.Menu.Menu_Callback;  
      end record; 

   type Menu_Listener_Ref is access all Menu_Listener_Typ'Class; 
   
   function New_Menu_Listener (
         Callback : Mcc.Gui.Menu.Menu_Callback;        
         This     : Menu_Listener_Ref             := null ) 
     return Menu_Listener_Ref; 

   procedure menuSelected (
      This : access Menu_Listener_Typ;
      P1   : access Javax.Swing.Event.MenuEvent.Typ'Class);

   procedure menuDeselected (
      This : access Menu_Listener_Typ;
      P1   : access Javax.Swing.Event.MenuEvent.Typ'Class);

   procedure menuCanceled (
      This : access Menu_Listener_Typ;
      P1   : access Javax.Swing.Event.MenuEvent.Typ'Class);
   -----------------------------------------------------------
   -- use Scale_Listener to respond to scale events
   -- try to follow JGNAT naming conventions
   -----------------------------------------------------------
   type Scale_Listener_Typ(
      I_ChangeListener : access Javax.Swing.Event.ChangeListener.Typ'Class)
      is new Java.Lang.Object.Typ with 
      record 
         Scale : Mcc.Gui.Widget.Scale.Scale_Pointer;  
      end record; 

   type Scale_Listener_Ref is access all Scale_Listener_Typ'Class; 
   
   function New_Scale_Listener (
         Mcc_Object : Mcc.Gui.Widget.Scale.Scale_Pointer;        
         This       : Scale_Listener_Ref             := null ) 
     return Scale_Listener_Ref; 

   procedure stateChanged (
      This : access Scale_Listener_Typ;
      P1   : access Javax.Swing.Event.ChangeEvent.Typ'Class);
   
private
   pragma Convention(Java,Window_Adapter_Typ);
   pragma Java_Constructor(New_Window_Adapter);
   pragma Export(Java,windowClosing,"windowClosing");
   pragma Export(Java,windowClosed,"windowClosed");

   pragma Convention(Java,Component_Adapter_Typ);
   pragma Java_Constructor(New_Component_Adapter);
   pragma Export(Java,componentResized,"componentResized");

   pragma Convention(Java,Key_Adapter_Typ);
   pragma Java_Constructor(New_Key_Adapter);
   pragma Export(Java,keyPressed,"keyPressed");
   pragma Export(Java,keyReleased,"keyReleased");

   pragma Convention(Java,Mouse_Adapter_Typ);
   pragma Java_Constructor(New_Mouse_Adapter);
   pragma Export(Java,mousePressed,"mousePressed");
   pragma Export(Java,mouseReleased,"mouseReleased");

   pragma Convention(Java,Mouse_Motion_Adapter_Typ);
   pragma Java_Constructor(New_Mouse_Motion_Adapter);
   pragma Export(Java,mouseDragged,"mouseDragged");
   pragma Export(Java,mouseMoved,"mouseMoved");

   pragma Convention(Java,Button_Action_Listener_Typ);
   pragma Java_Constructor(New_Button_Action_Listener);
   pragma Export(Java,actionPerformed,"actionPerformed");

   pragma Convention(Java,Menu_Action_Listener_Typ);
   pragma Java_Constructor(New_Menu_Action_Listener);
   -- already have below pragma from Buttons
   -- pragma Export(Java,actionPerformed,"actionPerformed");

   pragma Convention(Java,Menu_Listener_Typ);
   pragma Java_Constructor(New_Menu_Listener);
   pragma Export(Java,menuCanceled,"menuCanceled");
   pragma Export(Java,menuSelected,"menuSelected");
   pragma Export(Java,menuDeselected,"menuDeselected");


   pragma Convention(Java,Scale_Listener_Typ);
   pragma Java_Constructor(New_Scale_Listener);
   pragma Export(Java,stateChanged,"stateChanged");
end Peer.Listeners;
