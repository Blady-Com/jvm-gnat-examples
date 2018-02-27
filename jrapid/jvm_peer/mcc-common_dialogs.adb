-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER         
--           MCC-COMMON_DIALOGS
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
with Java.Awt.Component;
with Java.Io.File;
with Javax.Swing.JOptionPane;
with Javax.Swing.JFileChooser;
with Java_Strings;
with Peer.FileFilter;
with Java.Lang.System;
with Java.Lang.String;

package body Mcc.Common_Dialogs is
   type Dialog_Type is (Open,Save);
   Have_Saved_Directory : Boolean := False;
   Saved_Directory : Java.Io.File.Ref;
   
   ------------
   -- Ok_Box --
   ------------

   procedure Ok_Box
     (Message       : in String)
   is
   begin
      declare
         Null_Component : Java.Awt.Component.Ref := null;
      begin
         Javax.Swing.JOptionPane.showMessageDialog(
            Null_Component,
            Java_Strings.To_Java_String(Message));
      end;
   end Ok_Box;

   procedure Add_Filters(
         File_Types   : in String;
         Chooser      : in Javax.Swing.JFileChooser.Ref) is
      Current           : Natural := File_Types'First;
      Start_Description : Natural;
      End_Description   : Natural;
      Start_Extension   : Natural;
      End_Extension     : Natural;
      Done              : Boolean;
      Filter            : Peer.FileFilter.Ref;
      First_Filter      : Peer.FileFilter.Ref;
      use type Peer.FileFilter.Ref;
   begin
      while Current < File_Types'Last loop
         -- loop until we either get a { marking a filter start, or
         -- the end of the string
         while File_Types(Current) /= '{' and Current < File_Types'Last loop
            Current := Current + 1;
         end loop;
         
         -- if we're not at the end yet, process this filter.
         if Current < File_Types'Last then
            Filter := null;
            
            -- look for a " to begin the description
            while File_Types(Current) /= '"' loop
               Current := Current + 1;
            end loop;
            
            Current := Current + 1;
            Start_Description := Current;
            
            -- look for a " to end the description
            while File_Types(Current) /= '"' loop
               Current := Current + 1;
            end loop;
            
            End_Description := Current - 1;
            Done := False;
            -- look for the filter extensions
            while not Done loop
               while File_Types(Current) /= '*' and
                  File_Types(Current) /= '.' and
                  File_Types(Current) /= '}' loop
                  Current := Current + 1;
               end loop;
               
               -- if we hit } we're done,
               -- o/w, we saw * or ., which starts an extension
               if File_Types(Current) = '}' then
                  Done := True;
               elsif File_Types(Current) = '.' then
                  Start_Extension := Current+1;
                  Current := Current+1;
                  while File_Types(Current) /= ' ' and
                     File_Types(Current) /= '}' loop
                     Current := Current + 1;
                  end loop;
                  End_Extension := Current - 1;
                  if Filter = null then
                     Filter := Peer.FileFilter.new_FileFilter(
                        File_Types(Start_Description..End_Description));
                  end if;
                  
                  if First_Filter = null then
                     First_Filter := Filter;
                  end if;
                  
                  Peer.FileFilter.Add_Extension(
                     Filter,
                     File_Types(Start_Extension..End_Extension));
               else
                  Javax.Swing.JFileChooser.addChoosableFileFilter(
                     Chooser,
                     Javax.Swing.JFileChooser.getAcceptAllFileFilter(
                        Chooser));
                  -- move past the * filter
                  Current := Current + 1;
               end if;
            end loop;
            
            if Filter /= null then
               Javax.Swing.JFileChooser.addChoosableFileFilter(
                  Chooser,
                  Filter);
            end if;
         end if;
      end loop;
      
      -- make sure that the first filter is displayed
      -- as Java seems to put them in reverse order?
      if First_Filter /= null then
         Javax.Swing.JFileChooser.setFileFilter(
            Chooser,
            First_Filter);
      end if;
   end Add_Filters;

   procedure Common_Dialog(   
      File_Types        : in     String;
      Initial_Directory : in     String;
      Title             : in     String;
      Chooser           :    out Javax.Swing.JFileChooser.Ref;
      Filename          :    out String;
      File_Last         :    out Natural;
      Mode              : in     Dialog_Type;
      Change_Directory  : in     Boolean) is
      use type Java.Int;
      The_File : Java.Io.File.Ref;
      use type Java.Io.File.Ref;
      Result   : Java.Int;
   begin
      Chooser := Javax.Swing.JFileChooser.new_JFileChooser(
         Java_Strings.To_Java_String(Initial_Directory));
         
      if Have_Saved_Directory then
         begin
            Javax.Swing.JFileChooser.setCurrentDirectory(
               Chooser,
               Saved_Directory);
         exception
            when others => null; -- what if directory went away?
         end;
      end if;
      
      Add_Filters(
         File_Types => File_Types,
         Chooser    => Chooser);
      Javax.Swing.JFileChooser.setDialogTitle(
         Chooser,
         Java_Strings.To_Java_String(Title));
      declare
         Null_Component : Java.Awt.Component.Ref := null;
      begin
         if Mode = Open then
            Result := Javax.Swing.JFileChooser.showOpenDialog(
               Chooser,
               Null_Component);
         else
            Result := Javax.Swing.JFileChooser.showSaveDialog(
               Chooser,
               Null_Component);
         end if;
      end;
      
      if Result = Javax.Swing.JFileChooser.APPROVE_OPTION then
         The_File := Java.Io.File.Ref(Javax.Swing.JFileChooser.getSelectedFile(
            Chooser));
         
         if The_File /= null then
            declare
               Filename_String : String := Java_Strings.To_Ada_String(
                  Java.Lang.String.Ref(Java.Io.File.getAbsolutePath(The_File)));
            begin
               Filename(Filename'First..Filename'First+
                  Filename_String'Length-1) := Filename_String;
               File_Last := Filename'First+Filename_String'Length-1;
            end;

            if Change_Directory then
               Have_Saved_Directory := True;
               Saved_Directory := Java.Io.File.Ref(Java.Io.File.getParentFile(
                  The_File));
            end if;
         end if;

      end if;
   end Common_Dialog;
   -----------------
   -- Open_Dialog --
   -----------------

   procedure Open_Dialog
     (File_Types        : in String;
      Filename          : out String;
      File_Last         : out Natural;
      Title             : in String := "Open";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True)
   is
      Chooser : Javax.Swing.JFileChooser.Ref;
   begin
      Common_Dialog(
         File_Types        => File_Types,
         Initial_Directory => Initial_Directory,
         Title             => Title,
         Chooser           => Chooser,
         Filename          => Filename,
         File_Last         => File_Last,
         Mode              => Open,
         Change_Directory  => Change_Directory);
   end Open_Dialog;

   -----------------
   -- Open_Dialog --
   -----------------

   procedure Open_Dialog
     (File_Types : in String;
      Filename   : out String;
      File_Last  : out Natural;
      Directory  : out String;
      Dir_Last   : out Natural;
      Title      : in String := "Open";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True)
   is
      Chooser : Javax.Swing.JFileChooser.Ref;
   begin
      Common_Dialog(
         File_Types        => File_Types,
         Initial_Directory => Initial_Directory,
         Title             => Title,
         Chooser           => Chooser,
         Filename          => Filename,
         File_Last         => File_Last,
         Mode              => Open,
         Change_Directory  => Change_Directory);
      if File_Last >= Filename'First then
         declare
            Directory_String : String := Java_Strings.To_Ada_String(
               Java.Lang.String.Ref(Java.Io.File.getParent(
                  Javax.Swing.JFileChooser.getSelectedFile(Chooser))));
         begin
            Directory(Directory'First..Directory'First+
               Directory_String'Length-1) := Directory_String;
            Dir_Last := Directory'First+Directory_String'Length-1;
         end;
      else
         Dir_Last := Directory'First-1;
      end if;
   end Open_Dialog;

   -----------------
   -- Quit_Dialog --
   -----------------

   procedure Quit_Dialog
     (Verify : Boolean := True)
   is
      Answer : Java.Int;
      use type Java.Int;
   begin
      if Verify then
         declare
            Null_Component : Java.Awt.Component.Ref := null;
         begin
            Answer := Javax.Swing.JOptionPane.showConfirmDialog(
               Null_Component,
               Java_Strings.To_Java_String(
                  "Exit now?"),
               Java_Strings.To_Java_String(
                  "Are you sure you want to quit?"),
               Javax.Swing.JOptionPane.YES_NO_OPTION,
               Javax.Swing.JOptionPane.QUESTION_MESSAGE);
            if Answer = Javax.Swing.JOptionPane.YES_OPTION then
               Java.Lang.System.Exit_K(0);
            end if;
         end;
      else
         Java.Lang.System.Exit_K(0);
      end if;
   end Quit_Dialog;

   -----------------
   -- Save_Dialog --
   -----------------

   procedure Save_Dialog
     (File_Types : in String;
      Filename   : out String;
      File_Last  : out Natural;
      Title      : in String := "Save As";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True)
   is
      Chooser : Javax.Swing.JFileChooser.Ref;
   begin
      Common_Dialog(
         File_Types        => File_Types,
         Initial_Directory => Initial_Directory,
         Title             => Title,
         Chooser           => Chooser,
         Filename          => Filename,
         File_Last         => File_Last,
         Mode              => Save,
         Change_Directory  => Change_Directory);
   end Save_Dialog;

   -----------------
   -- Save_Dialog --
   -----------------

   procedure Save_Dialog
     (File_Types : in String;
      Filename   : out String;
      File_Last  : out Natural;
      Directory  : out String;
      Dir_Last   : out Natural;
      Title      : in String := "Save As";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True)
   is
      Chooser : Javax.Swing.JFileChooser.Ref;
   begin
      Common_Dialog(
         File_Types        => File_Types,
         Initial_Directory => Initial_Directory,
         Title             => Title,
         Chooser           => Chooser,
         Filename          => Filename,
         File_Last         => File_Last,
         Mode              => Save,
         Change_Directory  => Change_Directory);
      if File_Last >= Filename'First then
         declare
            Directory_String : String := Java_Strings.To_Ada_String(
               Java.Lang.String.Ref(Java.Io.File.getParent(
                  Javax.Swing.JFileChooser.getSelectedFile(Chooser))));
         begin
            Directory(Directory'First..Directory'First+
               Directory_String'Length-1) := Directory_String;
            Dir_Last := Directory'First+Directory_String'Length-1;
         end;
      else
         Dir_Last := Directory'First-1;
      end if;
   end Save_Dialog;

   ------------------
   -- Yesno_Dialog --
   ------------------

   function Yesno_Dialog
     (Message       : in String;
      Title         : in String)
      return Boolean
   is
      Answer : Java.Int;
      use type Java.Int;
   begin
      declare
         Null_Component : Java.Awt.Component.Ref := null;
      begin
         Answer := Javax.Swing.JOptionPane.showConfirmDialog(
            Null_Component,
            Java_Strings.To_Java_String(
               Title),
            Java_Strings.To_Java_String(
               Message),
            Javax.Swing.JOptionPane.YES_NO_OPTION,
            Javax.Swing.JOptionPane.QUESTION_MESSAGE);
      end;
      return (Answer = Javax.Swing.JOptionPane.YES_OPTION);
   end Yesno_Dialog;

end Mcc.Common_Dialogs;

