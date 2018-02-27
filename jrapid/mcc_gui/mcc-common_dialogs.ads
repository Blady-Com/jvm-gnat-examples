---------------------------------------------------------------
--                                                           
--  MCC-COMMON_DIALOGS.ADS 
--  Description : Description of Common Dialogs 
--                       
--  By: Martin Carlisle
--                                         
-- This is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- This is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
package Mcc.Common_Dialogs is
   -- Display message and wait for user to press "OK"
   procedure Ok_Box(
      Message       : in String);

   -- ask a yes or no question
   function Yesno_Dialog(
      Message       : in String;
      Title         : in String) return Boolean;

   -- Ask user if they want to quit if verify is true, then quit.  
   -- O/W, just quit.
   procedure Quit_Dialog(
      Verify : Boolean := True);

   -- Ask user for filename to open
   -- File_Types should look like :
   --   { "Tcl_Files" { .tcl .tk } }
   --   { "All Files" * }
   -- Note that Directory is the tail end of the Filename
   -- string (i.e. Filename is a fully qualified path)
   procedure Open_Dialog(
      File_Types        : in String;
      Filename          : out String;
      File_Last         : out Natural;
      Title             : in String := "Open";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True);

   -- Ask user for filename to open
   procedure Open_Dialog(
      File_Types : in String;
      Filename   : out String;
      File_Last  : out Natural;
      Directory  : out String;
      Dir_Last   : out Natural;
      Title      : in String := "Open";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True);

   -- Ask user for filename to Save
   procedure Save_Dialog(
      File_Types : in String;
      Filename   : out String;
      File_Last  : out Natural;
      Title      : in String := "Save As";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True);

   -- Ask user for filename to Save
   -- Note that Directory is the tail end of the Filename
   -- string (i.e. Filename is a fully qualified path)
   procedure Save_Dialog(
      File_Types : in String;
      Filename   : out String;
      File_Last  : out Natural;
      Directory  : out String;
      Dir_Last   : out Natural;
      Title      : in String := "Save As";
      Initial_Directory : in String := ".";
      Default_Extension : in String := "";
      Change_Directory  : in Boolean := True);
end Mcc.Common_Dialogs;
