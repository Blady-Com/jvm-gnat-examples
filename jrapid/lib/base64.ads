-------------------------------------------------------------
-- package base64
--
-- By: Martin C. Carlisle
--     United States Air Force Academy
--     Department of Computer Science
--
-- This file is free software; you can redistribute it and/or 
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
-------------------------------------------------------------

package base64 is
   -- takes a string of arbitrary characters and returns
   -- a base 64 encoding.
   function Encode(Input : in String) return String;
end base64;