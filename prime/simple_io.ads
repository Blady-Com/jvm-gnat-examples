
package Simple_IO is

   procedure New_Line;

   procedure Put (B : Boolean);
   procedure Put (C : Character);
   procedure Put (I : Integer);
   procedure Put (F : Float);
   procedure Put (S : String);

   procedure Put_Line (S : String);
   
   function Get_Int return Integer;
   --  Raises an above exception when bad integer format

private
   pragma Import (Java, New_Line, "new_line");
   pragma Import (Java, Put,      "put");
   pragma Import (Java, Put_Line, "put_line");
   pragma Import (Java, Get_Int,  "get_int");
end Simple_IO;

pragma Import (Java, Simple_IO, "simple_io");

