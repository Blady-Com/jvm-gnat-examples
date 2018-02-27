---------------------------------------------------------------
--                                                           
--  LISTS_GENERIC-GENERIC_KEY.ADB 
--  Description : Extension of Michael Feldman's Lists_Generic
--                package to include keys
--                       
--  By: Martin Carlisle
--                                         
-- LISTS_GENERIC-GENERIC_KEY is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- LISTS_GENERIC-GENERIC_KEY is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
package body Lists_Generic.Generic_Key is

   function Find(Key : Keytype; Ptr : List) return Position is
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

   function IsPresent(Key : Keytype; Ptr : List) return Boolean is
      Traverse : Position;
   begin
      Traverse := First(Ptr);
      while not IsPastEnd(L => Ptr, P => Traverse) loop
         if GetKey(Retrieve(Ptr,Traverse)) = Key then
            return True;
         end if;
         GoAhead(Ptr,Traverse);
      end loop;
      return False;
   end IsPresent;

end Lists_Generic.Generic_Key;
