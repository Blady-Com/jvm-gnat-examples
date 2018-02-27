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
with Javax.Swing.ButtonModel;
with Javax.Swing.AbstractButton;
with ada.unchecked_conversion;
package body peer.radio is
   function New_RadioGroup (
         This     : RadioGroup_Ref                             := null )
     return RadioGroup_Ref is
      Super : Javax.Swing.ButtonGroup.Ref := 
         Javax.Swing.ButtonGroup.new_ButtonGroup(This => 
            Javax.Swing.ButtonGroup.Ref(This));
   begin
      This.Radios := Java.Util.Vector.new_Vector;
      return This;
   end New_RadioGroup;   

   function New_RadioButton (
         Radio    : Mcc.Gui.Widget.Button.Radio.Radio_Pointer;   
         Text     : Java.Lang.String.Ref;
         Selected : Java.Boolean;     
         This     : RadioButton_Ref                             := null )
     return RadioButton_Ref is
      Super : Javax.Swing.JRadioButton.Ref := 
         Javax.Swing.JRadioButton.new_JRadioButton(
            Text,
            Selected,
            Javax.Swing.JRadioButton.Ref(This));
   begin
      This.Radio := Radio;
      return This;
   end New_RadioButton;   
end peer.radio;
