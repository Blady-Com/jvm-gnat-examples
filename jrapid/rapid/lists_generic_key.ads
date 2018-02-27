-- Lists_Generic_Key
--
-- contribution by Frederic Abiven to work around 3.10p bug
--
with Lists_Generic;
with Lists_Generic.Key;

generic

   type Element is private;
   type Keytype is private;
   with  function GetKey(Item : in Element) return Keytype;

package Lists_Generic_Key is

   package List_Package is new
     Lists_Generic(Element);

   package List_Package_Key is new
     List_Package.Key(Keytype,GetKey);

end Lists_Generic_Key;