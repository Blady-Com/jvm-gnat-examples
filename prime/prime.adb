with Simple_IO; use Simple_IO;

procedure Prime is

   type Table is array (Natural range <>) of Boolean;

   Num : Integer;

   procedure Erathostene (N : Natural) is
      Primes  : Table (2 .. N) := (others => True);
      Current : Natural := 2;
      Tmp     : Natural;
   begin
      while Current <= N loop
         Put (Current);
         Put (" ");

         Tmp := Current;
         while Tmp <= N loop
            Primes (Tmp) := False;
            Tmp := Tmp + Current;
         end loop;

         while Current <= N
           and then not Primes (Current)
         loop
            Current := Current + 1;
         end loop;
      end loop;
   end Erathostene;

begin
   Put ("Prime numbers between 1 and ? : ");
   Num := Get_Int;
   Erathostene (Num);
   Put_Line ("");
exception
   when others =>
      Put_Line ("Bad format - try again");
end Prime;
