---------------------------------------------------------------
--                                                           
--  GENERATE_HELPERS 
--  Description : Use to auto generate either Tcl or Ada (TASH) code
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
with Ada.Text_IO;

package Generate_Helpers is
   -- procedure that removes parenthesis within a procedure call
   procedure Generate_Removed_Parameters(File : in Ada.Text_IO.File_Type;
      Action : in String);

   -- generate context clause needed for action
   procedure Generate_With(File : in Ada.Text_IO.File_Type;
         Action     : in String);

   -- return name with "." converted to "_"
   function Undot_Name(Name : in String) return String;

   -- make sure every " inside is ""
   function Quote_String(Item : in String) return String;
end Generate_Helpers;