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
with Peer;
package Mcc.Gui is
   Unimplemented : exception;
   -- the Unimplemented exception will be raised for any subprogram
   -- that is not implemented in the Peer

   ------------------------------------------------
   -- color type
   --
   -- see mcc-gui-colors for some color constants
   ------------------------------------------------
   type Intensity is new Natural range 0..255;
   type Color is record
      Red   : Intensity;
      Green : Intensity;
      Blue  : Intensity;
   end record;

   ------------------------------------------------
   -- Key Listener types
   --
   -- A key listener can be attached to any object
   -- it will receive a key event whenever a key
   -- is either pressed or released in that
   -- object.  See package mcc.gui.keys for
   -- common key constants
   ------------------------------------------------
   type Key_Action_Type is (Press,Release);
   type Key_Type is new Natural;

   type Key_Event is record
      Key    : Key_Type;
      Action : Key_Action_Type;
   end record;


   ------------------------------------------------
   -- Mouse Listener types
   --
   -- A mouse listener can be attached to any object
   -- it will receive a mouse event whenever the mouse
   -- is moved or a button pressed/released in that
   -- object
   ------------------------------------------------
   type Mouse_Action_Type is (Press,Move,Release,Double_Click);
   type Mouse_Button is (None,Left,Middle,Right);

   type Mouse_Event is record
      Button : Mouse_Button;
      Action : Mouse_Action_Type;
      X      : Integer;
      Y      : Integer;
   end record;

   ------------------------------------------------
   -- type Object -- the root of the hierarchy
   ------------------------------------------------
   type Object is abstract tagged private;
   type Object_Pointer is access all Object'Class;

   ------------------------------------------------
   -- procedure Destroy
   --
   -- destroys an object
   ------------------------------------------------
   procedure Destroy(Obj : in Object);

   ------------------------------------------------
   -- function Get_Peer
   --
   -- each implementation of the GUI library
   -- has a peer element which implements the
   -- object.  This function returns that peer
   -- so that the user could call native methods
   -- for features not implemented in mcc.gui
   ------------------------------------------------
   function Get_Peer(Obj : in Object) return Peer.Peer;

   type Sized_Object is abstract new Object with private;
   type Sized_Object_Pointer is access all Sized_Object'Class;
   
   ------------------------------------------------
   -- procedure Resize
   --
   -- change the Width and Height of an Object
   ------------------------------------------------
   procedure Resize(
      Obj    : in out Sized_Object;
      Width  : in     Integer;
      Height : in     Integer);

   ------------------------------------------------
   -- procedure Move
   --
   -- Move the object to a new location
   -- this location is based on the parent container
   -- or the screen for the main window
   ------------------------------------------------
   procedure Move(
      Obj : in out Sized_Object;
      X   : in     Integer;
      Y   : in     Integer);

   ------------------------------------------------
   -- procedure Set_Background_Color
   --
   -- change the background color for an object
   ------------------------------------------------
   procedure Set_Background_Color(
      Obj       : in out Sized_Object;
      New_Color : in     Color);

   ------------------------------------------------
   -- procedure Set_Key_Listener
   --
   -- attaches a key listener to this object
   -- see above for description
   ------------------------------------------------
   type Key_Listener is access procedure(
      Obj   : in out Sized_Object'Class;
      Event : in     Key_Event);

   procedure Set_Key_Listener(
      Obj      : in Sized_Object_Pointer;
      Listener : in Key_Listener);
   function Get_Key_Listener(
      Obj : in Sized_Object'Class) return Key_Listener;

   ------------------------------------------------
   -- procedure Set_Mouse_Listener
   --
   -- attaches a mouse listener to this object
   -- see above for description
   ------------------------------------------------
   type Mouse_Listener is access procedure(
      Obj   : in out Sized_Object'Class;
      Event : in     Mouse_Event);

   procedure Set_Mouse_Listener(
      Obj      : in Sized_Object_Pointer;
      Listener : in Mouse_Listener);
   function Get_Mouse_Listener(
      Obj : in Sized_Object'Class) return Mouse_Listener;

   type Cursor_Type is (
      Default_Cursor, 
      Resize_NW, 
      Resize_N,
      Resize_NE,
      Resize_E,
      Resize_SE,
      Resize_S,
      Resize_SW,
      Resize_W,
      Move_Cursor);
      
   ------------------------------------------------
   -- procedure Set_Cursor
   --
   -- change which cursor will be displayed when
   -- mouse is over this object
   ------------------------------------------------
   procedure Set_Cursor(
      Obj    : in Sized_Object;
      Cursor : in Cursor_Type);

   ------------------------------------------------
   -- procedure Bell
   --
   -- ring bell
   ------------------------------------------------
   procedure Bell;
private
   type Object is abstract tagged record
      My_Peer       : Peer.Peer;
   end record;

   type Sized_Object is abstract new Object with record
      Key_Handler   : Key_Listener   := null;
      Mouse_Handler : Mouse_Listener := null;
   end record;

end Mcc.Gui;