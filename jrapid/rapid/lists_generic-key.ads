---------------------------------------------------------------
--                                                           
--  LISTS_GENERIC-KEY.ADS 
--  Description : Extension of Michael Feldman's Lists_Generic
--                package to include keys
--                       
--  By: Martin Carlisle
--                                         
-- LISTS_GENERIC-KEY is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- LISTS_GENERIC-KEY is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
-- This requires each list item to have a string which is the
-- key.  It allows lookups based on that key.
--
-- This extends Feldman's functionality to add something to
-- do a find and check if it is present.
---------------------------------------------------------------
generic
   with function GetKey (Item : ElementType) return String;


package Lists_Generic.Key is

   Not_Found : exception;

   -- returns position of 1st item in list containing key
   -- raised Not_Found if there is no such item
   function Find(Key : String; Ptr : List) return Position;
   
   -- return true if there is an item in list with given key
   function IsPresent(Key : String; Ptr : List) return Boolean;

end Lists_Generic.Key;