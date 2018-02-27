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
--
-- This is a derivative work based on a copyrighted work by Sun
-- see below for its copyright:
--
-- @(#)ExampleFileFilter.java   1.9 99/04/23
--
-- Copyright (c) 1998, 1999 by Sun Microsystems, Inc. All Rights Reserved.
--
-- Sun grants you ("Licensee") a non-exclusive, royalty free, license to use,
-- modify and redistribute this software in source and binary code form,
-- provided that i) this copyright notice and license appear on all copies of
-- the software; and ii) Licensee does not utilize the software in a manner
-- which is disparaging to Sun.
--
-- This software is provided "AS IS," without a warranty of any kind. ALL
-- EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING AN
-- IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR
-- NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE
-- LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING
-- OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR IT
-- LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT,
-- INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
-- CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF
-- OR INABILITY TO USE SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGES.
--
-- This software is not designed or intended for use in on-line control of
-- aircraft, air traffic, aircraft navigation or aircraft communications; or i
-- the design, construction, operation or maintenance of any nuclear
-- facility. Licensee represents and warrants that it will not use or
-- redistribute the Software for such purposes.
------------------------------------------------------------------------------
with Java_Strings;
with Ada.Characters.Handling;
with Java.Util.Enumeration;
package body Peer.FileFilter is
   
   -- allocator
   function New_FileFilter (
         Description : in String;
         This        : in Ref     := null) return Ref is
      Super : Javax.Swing.FileChooser.FileFilter.Ref := 
         Javax.Swing.FileChooser.FileFilter.new_FileFilter(
            Javax.Swing.FileChooser.FileFilter.Ref(This));
   begin
      This.Extensions  := Java.Util.Hashtable.new_Hashtable(5);
      This.Description := Java_Strings.To_Java_String(
         Description);
      This.Full_Description := null;
      return This;
   end New_FileFilter;

   -- Add_Extension
   procedure Add_Extension (
         This      : access Typ; 
         Extension : in     String) is
      Ignore : Java.Lang.Object.Ref;
   begin
      Ignore := Java.Lang.Object.Ref(Java.Util.Hashtable.put(
         Java.Util.Hashtable.Ref(This.Extensions),
         Java.Lang.Object.Ref(
            Java_Strings.To_Java_String(
               Ada.Characters.Handling.To_Lower(Extension))),
         Java.Lang.Object.Ref(this)));
      This.Full_Description := null;
   end Add_Extension;

   function Get_Extension(f : access Java.Io.File.Typ'Class) 
         return Java.Lang.String.Ref is
      filename : Java.Lang.String.Ref;
      I        : Java.Int;
   begin
      filename := Java.Lang.String.Ref(Java.Io.File.getName(f));
      i := Java.Lang.String.lastIndexOf(
         filename,
         Java.int(Character'Pos('.')));
      if i > 0 and i < Java.Lang.String.length(filename)-1 then
         return Java.Lang.String.Ref(Java.Lang.String.toLowercase(
            Java.Lang.String.substring(filename,i+1)));
      end if;
      return null;
   end Get_Extension;
         
   -- accept (take a file and report whether it passes the filter)
   function accept_K(
         This : access Typ;
         F    : access Java.Io.File.Typ'Class) return Java.Boolean is
      use type Java.Io.File.Ref;
      use type Java.Lang.String.Ref;
      use type Java.Lang.Object.Ref;
      Extension : Java.Lang.String.Ref;
   begin
      if Java.Io.File.isDirectory(f) then
         return true;
      end if;
         
      Extension := get_Extension(f);
      if (extension /= null and then 
         Java.Util.Hashtable.get(
            This.Extensions,Extension) /= null) then
         return true;
      end if;
      return false;
   end accept_K;
         
   -- return the text for the file choices box
   function getDescription(
         This : access Typ) return access Java.Lang.String.Typ'Class is
      use type Java.Lang.String.Ref;
--      use type Java.Lang.String.Typ;
--      use type Java.Util.Hashtable.Ref;
      use type Java.Util.Enumeration.Ref;
      Extensions : Java.Util.Enumeration.Ref;
   begin
      if This.Full_Description = null then
         This.Full_Description := 
            Java.Lang.String.Ref(Java.Lang.String.concat(
               This.Description,
               Java_Strings.To_Java_String(" (")));
         Extensions := Java.Util.Enumeration.Ref(Java.Util.Hashtable.Keys(
            This.Extensions));
         if Extensions /= null then
            This.Full_Description := 
               Java.Lang.String.Ref(Java.Lang.String.concat(
                  This.Full_Description,
                  Java.Lang.String.concat(
                     Java_Strings.To_Java_String("*."),
                     Java.Lang.String.Ref(
                        Java.Util.Enumeration.nextElement(
                           Extensions)))));
            while Java.Util.Enumeration.hasMoreElements(
                  Extensions) loop
               This.Full_Description := 
                  Java.Lang.String.Ref(Java.Lang.String.concat(
                     This.Full_Description,
                     Java.Lang.String.concat(
                        Java_Strings.To_Java_String(", "),
                        Java.Lang.String.Ref(
                           Java.Util.Enumeration.nextElement(
                              Extensions)))));
            end loop;
         end if;
         This.Full_Description := Java.Lang.String.Ref(Java.Lang.String.concat(
            This.Full_Description,
            Java_Strings.To_Java_String(")")));
      end if;
      return This.Full_Description;
   end getDescription;

   -- change the description
   procedure setDescription(
         This        : access Typ;
         Description : in     String) is
   begin
      This.Description := Java_Strings.To_Java_String(
         Description);
      This.Full_Description := null;
   end setDescription;
         
end Peer.FileFilter;