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
package Mcc.Gui.Container.Window is
   type Window is abstract new Container with private;
   type Window_Pointer is access all Window'Class;

   ------------------------------------------------
   -- procedure Set_Title
   --
   -- change Titlebar of a Window
   ------------------------------------------------
   procedure Set_Title(
      Obj   : in out Window;
      Title : in     String);

   ------------------------------------------------
   -- procedure Set_Close_Handler
   --
   -- perform this action when window is about to
   -- close.  If a close handler is set, the window
   -- will not close unless the handler forces it
   -- to.
   ------------------------------------------------
   type Close_Handler is access procedure(Obj : in out Window'Class);
   procedure Set_Close_Handler(
      Obj     : in Window_Pointer;
      Handler : in Close_Handler);
   function Get_Close_Handler(
      Obj : in Window'Class) return Close_Handler;

   ------------------------------------------------
   -- procedure Set_Resize_Handler
   --
   -- perform this action when window is about to
   -- be resized.
   ------------------------------------------------
   type Resize_Handler is access procedure(
      Obj    : in out Window'Class;
      Width  : in     Integer;
      Height : in     Integer);
   procedure Set_Resize_Handler(
      Obj     : in Window_Pointer;
      Handler : in Resize_Handler);
   function Get_Resize_Handler(
      Obj : in Window'Class) return Resize_Handler;

   ------------------------------------------------
   -- procedure To_Top
   --
   -- causes a window to be displayed on top
   ------------------------------------------------
   procedure To_Top(Obj : in Window);

   ------------------------------------------------
   -- procedure Show
   --
   -- causes a window to become visible if not
   -- already
   ------------------------------------------------
   procedure Show(Obj : in Window);

   ------------------------------------------------
   -- procedure Hide
   --
   -- causes a window to become invisible if not
   -- already
   ------------------------------------------------
   procedure Hide(Obj : in Window);

   ------------------------------------------------
   -- the Main_Window, when closed, will cause
   -- the event loop to terminate
   ------------------------------------------------
   type Main_Window is new Window with private;

   ------------------------------------------------
   -- procedure Create
   --
   -- open the window
   ------------------------------------------------
   procedure Create(
      Obj    : in out Main_Window;
      X      : in     Natural;
      Y      : in     Natural;
      Width  : in     Natural;
      Height : in     Natural);

   type Subwindow is new Window with private;
   ------------------------------------------------
   -- procedure Create
   --
   -- open the window
   ------------------------------------------------
   procedure Create(
      Obj    : in out Subwindow;
      X      : in     Natural;
      Y      : in     Natural;
      Width  : in     Natural;
      Height : in     Natural);

   ------------------------------------------------
   -- procedure Event Loop
   --
   -- call this to transfer control to the event
   -- loop.  This procedure does not exit until the
   -- main window is closed.
   ------------------------------------------------
   procedure Event_Loop;

private
   type Window is new Container with record
      Resize_Callback : Resize_Handler;
      Close_Callback  : Close_Handler;
   end record;

   type Main_Window is new Window with null record;
   type Subwindow is new Window with null record;
end Mcc.Gui.Container.Window;