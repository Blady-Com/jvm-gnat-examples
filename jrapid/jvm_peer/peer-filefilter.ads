-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           JVM PEER IMPLEMENTATION                               
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
with Javax.Swing.FileChooser.FileFilter;
with Java.Util.Hashtable;
with Java.Lang.String;
with Java.Io.File;
package Peer.FileFilter is
   type Typ is new Javax.Swing.FileChooser.FileFilter.Typ with 
      record 
         Extensions       : Java.Util.Hashtable.Ref;  
         Description      : Java.Lang.String.Ref;  
         Full_Description : Java.Lang.String.Ref;  
      end record; 

   type Ref is access all Typ'Class; 

   -- allocator
   function New_FileFilter (
         Description : in String;
         This        : in Ref     := null) return Ref;

   -- Add_Extension
   procedure Add_Extension (
         This      : access Typ; 
         Extension : in     String);
         
   -- accept (take a file and report whether it passes the filter)
   function accept_K(
         This : access Typ;
         F    : access Java.Io.File.Typ'Class) return Java.Boolean;
         
   -- return the text for the file choices box
   function getDescription (This : access Typ)
                            return access Java.Lang.String.Typ'Class;

   -- change the description of the file
   procedure setDescription(
         This        : access Typ;
         Description : in     String);         
private
   pragma Convention(Java,Typ);  
   pragma Java_Constructor(New_FileFilter);
   pragma Export(Java,Accept_K,"accept");
   pragma Export(Java,getDescription,"getDescription");
end Peer.FileFilter;