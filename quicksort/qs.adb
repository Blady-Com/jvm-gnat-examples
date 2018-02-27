with Quick_Sort, Util, Ada.Text_IO;
use Quick_Sort, Util, Ada.Text_IO;

procedure QS is

   N : Integer;

   package Int_IO is new Integer_IO (Integer);

   use Int_IO;

   procedure Init (B : in out Items) is
   begin
      for J in B'Range loop
         B (J) := Random rem 500 + 500;
      end loop;
    end Init;

   procedure Print (B : Items) is
   begin
      for J in B'range loop
         Put (B (J), Width => 0);
         Put (" ");
      end loop;
   end Print;

begin
   Put ("Quick sort demo. How many numbers ? ");
   Get (N);

   declare
      Buf : Items (1 .. N);
   begin
      Init (Buf);
      Put_Line ("Sorting: ");
      Print (Buf);
      New_Line;
      Sort (Buf);
      Put_Line ("Result: ");
      Print (Buf);
      New_Line;
   end;
end QS;
