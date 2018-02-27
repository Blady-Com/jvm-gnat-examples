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
-- These routines do inefficient copying and will be replaced when
-- the intrinsics defined in Java.Lang.String become available
------------------------------------------------------------------------------
with Java.Lang.String;

with Ada.Unchecked_Conversion;
package body Java_Strings is
   function Convert is new Ada.Unchecked_Conversion(
      Character,Java.Byte);
      
   function To_Java_String(x : in String) return Java.Lang.String.Ref is
      y : Java.Byte_Arr := new Java.Byte_Arr_Obj(0..x'length-1);
   begin
      for i in y'range loop
         y(i) := Convert(x(x'first+i));
      end loop;
      return Java.Lang.String.new_String(y);
   end To_Java_String;
   
   function To_Ada_String(x : in Java.Lang.String.Ref) return 
      String is
      Bytes : Java.Byte_Arr;
   begin
      Bytes := Java.Lang.String.getBytes(x);
      declare
         Result : String(1..Bytes.all'Length);
      begin
         for i in Result'range loop
            Result(i) := Character'Val(Bytes(Bytes.all'First+i-1));
         end loop;
         return Result;
      end;
   end To_Ada_String;
end Java_Strings;
