---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  LIMITS.ADS 
--  Description : upper bounds on what we can do
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
-- Use this package for global constants that represent limits
-- on sizes, etc.
--
-- Makes it easy to find limitations, increase abilities.
---------------------------------------------------------------
package Limits is
   -- used for filenames
   Max_Name_Length : constant := 255;

   -- used for widget names
   Max_Word_Length : constant := 80;

   Min_Rapid_Window_Width  : constant := 300;
   Min_Rapid_Window_Height : constant := 100;
end Limits;
