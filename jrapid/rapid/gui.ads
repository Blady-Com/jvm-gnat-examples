---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI.ADS 
--  Description : Root of GUI Hierarchy
--                       
--  By: Martin Carlisle
--                                         
-- RAPID is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- RAPID is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
-- Contains base types, but no objects
---------------------------------------------------------------
with Limits;
package gui is

   
   type String_Pointer is access all String;
   -- 
   subtype Word_Type is String(1..Limits.Max_Word_Length);

end gui;