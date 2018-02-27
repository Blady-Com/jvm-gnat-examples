with Ada.Text_IO;  use Ada.Text_IO;

procedure Demo is
   F : File_Type;
   type Foo is (AA, BB, CC);
   pragma Warnings (Off, Foo);

   type Foo2 is range 0 .. 10;

   package Foo_IO is new Enumeration_IO (Foo);
   package Int_IO is new Integer_IO (Foo2);
   B : Foo;
begin

   Put ("Testing Enumeration_IO :");
   B := BB;
   Foo_IO.Put (B);
   Put_Line ("");

   Put ("Testing Integer_IO :");
   Int_IO.Put (5);
   Put_Line ("");

   Put_Line ("Opening a file and writing to it");
   Create (F, Append_File, "toto.txt");
   --   Open also works in the above statement

   Put (F, "aaa");
   Put_Line (F, "bbb");
   Put (F, "ccc");
   Close (F);

   Put_Line ("Reading from the file");

   declare
      S : String (1 .. 1024);
      Last : Natural;
   begin
      Open (F, In_File, "toto.txt");
      Get_Line (F, S, Last);
      Put_Line ("Read : ");
      Put_Line (S (1 .. Last));
      Close (F);

      Put ("Reading from stdin: please enter any string:");

      Get_Line (S, Last);
      Put_Line ("");
      Put_Line ("You just typed : " & S (1 .. Last));

   end;
end Demo;
