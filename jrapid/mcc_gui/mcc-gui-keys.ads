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
-----------------------------------------------------------------------------
with Peer.Keys;
package Mcc.Gui.Keys is
   -- these constants are used in the Key_Event record for a 
   -- Key_Listener in Mcc.Gui.  They get their values differently
   -- for each implementation
   Backspace : constant Key_Type := Key_Type(Peer.Keys.Backspace);
   Control   : constant Key_Type := Key_Type(Peer.Keys.Control);
   Enter     : constant Key_Type := Key_Type(Peer.Keys.Enter);
   Shift     : constant Key_Type := Key_Type(Peer.Keys.Shift);
   Tab       : constant Key_Type := Key_Type(Peer.Keys.Tab);
   Home      : constant Key_Type := Key_Type(Peer.Keys.Home);
   End_Key   : constant Key_Type := Key_Type(Peer.Keys.End_Key);
   Page_Up   : constant Key_Type := Key_Type(Peer.Keys.Page_Up);
   Page_Down : constant Key_Type := Key_Type(Peer.Keys.Page_Down);
   Ins       : constant Key_Type := Key_Type(Peer.Keys.Ins);
   Del       : constant Key_Type := Key_Type(Peer.Keys.Del);
   Up        : constant Key_Type := Key_Type(Peer.Keys.Up);
   Down      : constant Key_Type := Key_Type(Peer.Keys.Down);
   Left      : constant Key_Type := Key_Type(Peer.Keys.Left);
   Right     : constant Key_Type := Key_Type(Peer.Keys.Right);
   F1        : constant Key_Type := Key_Type(Peer.Keys.F1);
   F2        : constant Key_Type := Key_Type(Peer.Keys.F2);
   F3        : constant Key_Type := Key_Type(Peer.Keys.F3);
   F4        : constant Key_Type := Key_Type(Peer.Keys.F4);
   F5        : constant Key_Type := Key_Type(Peer.Keys.F5);
   F6        : constant Key_Type := Key_Type(Peer.Keys.F6);
   F7        : constant Key_Type := Key_Type(Peer.Keys.F7);
   F8        : constant Key_Type := Key_Type(Peer.Keys.F8);
   F9        : constant Key_Type := Key_Type(Peer.Keys.F9);
   F10       : constant Key_Type := Key_Type(Peer.Keys.F10);
   F11       : constant Key_Type := Key_Type(Peer.Keys.F11);
   F12       : constant Key_Type := Key_Type(Peer.Keys.F12);
end Mcc.Gui.Keys;