---------------------------------------------------------------
--                                                           
--  LISTS_GENERIC-KEY.ADB 
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
with Ada.Characters.Handling;
package body Lists_Generic.Key is

   function Find(Key : String; Ptr : List) return Position is
      Traverse : Position;
   begin
      Traverse := First(Ptr);
      while not IsPastEnd(L => Ptr, P => Traverse) loop
         if GetKey(Retrieve(Ptr,Traverse)) = Key then
            return Traverse;
         end if;
         GoAhead(Ptr,Traverse);
      end loop;
      raise Not_Found;
   end Find;

   function IsPresent(Key : String; Ptr : List) return Boolean is
      Traverse  : Position;
      Lower_Key : String := Ada.Characters.Handling.To_Lower(Key);
   begin
      Traverse := First(Ptr);
      while not IsPastEnd(L => Ptr, P => Traverse) loop
         if Ada.Characters.Handling.To_Lower(
               GetKey(Retrieve(Ptr,Traverse))) = Lower_Key then
            return True;
         end if;
         GoAhead(Ptr,Traverse);
      end loop;
      return False;
   end IsPresent;

end Lists_Generic.Key;
