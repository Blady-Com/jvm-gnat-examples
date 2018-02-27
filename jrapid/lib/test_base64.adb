with Ada.Direct_IO;
with Ada.Command_Line;
with Base64;
with Ada.Text_Io;
with System;
procedure test_base64 is
   package Character_IO is new Ada.Direct_IO(Character);
   use Character_IO;
   File : Character_IO.File_Type;
   
begin
   Open(File,In_File,Ada.Command_Line.Argument(1));
   
   declare
      Input : String(1..Integer(Size(File)));
   begin
      for i in Input'range loop
         Read(File,Input(i));
      end loop;
      declare
         Result : String := Base64.Encode(Input);
      begin
         Ada.Text_IO.Put_Line(Result);
      end;
   end;
end test_base64;