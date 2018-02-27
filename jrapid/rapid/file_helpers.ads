---------------------------------------------------------------
--                                                           
--  FILE_HELPERS.ADS 
--  Description : IO helpers
--                       
--  By: Martin Carlisle
--      United States Air Force Academy
--                                         
-- FILE_HELPERS is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- FILE_HELPERS is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------

with Ada.Text_Io;

package File_Helpers is

   ----------------------------------------------------
   -- PROCEDURE Get_String
   -- 
   -- reads from file the next string.  If first non-blank
   -- is ", then reads until ending ", o/w reads sequence
   -- of non-blank characters.  As in Ada, to embed a string,
   -- use "".  Stops at end of line.
   ----------------------------------------------------
   procedure Get_String(File : in Ada.Text_IO.File_Type;
         Item : out String; Last : out Natural);

   ----------------------------------------------------
   -- PROCEDURE Put_String
   -- 
   -- writes string to a file enclosed in "".  If string contains '"' 
   -- then two quotation marks are written.
   ----------------------------------------------------
   procedure Put_String(File : in Ada.Text_IO.File_Type;
         Item : in String);

end File_Helpers;