-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER
--           TCL PEER for the
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
-----------------------------------------------------------------------------
with Java.Awt.Event.KeyEvent;
use Java.Awt.Event.KeyEvent;
package Peer.Keys is
   Backspace : constant Java.Int := VK_BACK_SPACE;
   Control   : constant Java.Int := VK_CONTROL;
   Enter     : constant Java.Int := VK_ENTER;
   Shift     : constant Java.Int := VK_SHIFT;
   Tab       : constant Java.Int := VK_TAB;
   Home      : constant Java.Int := VK_HOME;
   End_Key   : constant Java.Int := VK_END;
   Page_Up   : constant Java.Int := VK_PAGE_UP;
   Page_Down : constant Java.Int := VK_PAGE_DOWN;
   Ins       : constant Java.Int := VK_INSERT;
   Del       : constant Java.Int := VK_DELETE;
   Up        : constant Java.Int := VK_UP;
   Down      : constant Java.Int := VK_DOWN;
   Left      : constant Java.Int := VK_LEFT;
   Right     : constant Java.Int := VK_RIGHT;
   F1        : constant Java.Int := VK_F1;
   F2        : constant Java.Int := VK_F2;
   F3        : constant Java.Int := VK_F3;
   F4        : constant Java.Int := VK_F4;
   F5        : constant Java.Int := VK_F5;
   F6        : constant Java.Int := VK_F6;
   F7        : constant Java.Int := VK_F7;
   F8        : constant Java.Int := VK_F8;
   F9        : constant Java.Int := VK_F9;
   F10       : constant Java.Int := VK_F10;
   F11       : constant Java.Int := VK_F11;
   F12       : constant Java.Int := VK_F12;
end Peer.Keys;
