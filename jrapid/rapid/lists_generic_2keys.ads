-- Lists_Generic_Key
--
-- contribution by Frederic Abiven to work around 3.10p bug
--
-- extended to allow two different types of lookup by Martin C. Carlisle
-- one string and one generic
with Lists_Generic;
with Lists_Generic.Key;
with Lists_Generic.Generic_Key;

generic

   type Element is private;
   with  function GetKey(Item : in Element) return String;
   type Key2type is private;
   with  function GetKey2(Item : in Element) return Key2type;

package Lists_Generic_2Keys is

   package List_Package is new
     Lists_Generic(Element);

   package List_Package_Key is new
     List_Package.Key(GetKey);

   package List_Package_Key2 is new
     List_Package.Generic_Key(Key2type,GetKey2);
end Lists_Generic_2Keys;
