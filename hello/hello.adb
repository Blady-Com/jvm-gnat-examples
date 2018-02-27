with Text_IO; use Text_IO;

procedure Hello  is
begin
   Put_Line ("Hello Java World. Here are some Text_IO examples:");
   New_Line;

   --  Tests for TextIO

   Put ('A');
   New_Line;

   Put ("A String");
   New_Line;

   Set_Col (10);
   Put ("On column 10");

   Set_Col (5); --  should go to next line
   Put_Line ("On column 5 (next line)");

   Put_Line ("Done :)");
end Hello;
