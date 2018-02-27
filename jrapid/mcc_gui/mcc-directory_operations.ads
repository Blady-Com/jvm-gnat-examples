---------------------------------------------------------------
--                                                           
--  MCC-DIRECTORY_OPERATIONS.ADS 
--  Description : Simple operations on directories 
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
with GNAT.Os_Lib;
with GNAT.Directory_Operations;
package Mcc.Directory_Operations is
   -- used in a path between directories
   -- '\' on Windows, '/' on UNIX
   Directory_Separator : constant Character := 
      GNAT.OS_Lib.Directory_Separator;
      
   -- change the present working directory
   procedure Change_Dir(Dir_Name : in String) renames
      GNAT.Directory_Operations.Change_Dir;
      
   -- get the present working directory
   -- requires GNAT 3.12a or later
   -- contains Directory_Separator at end (if platform has such a thing)
   -- so that you can simply append a filename
   function Get_Current_Dir return String renames
      GNAT.Directory_Operations.Get_Current_Dir;
end Mcc.Directory_Operations;