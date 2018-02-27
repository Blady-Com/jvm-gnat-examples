---------------------------------------------------------------
--                                                           
--  GENERATE_HELPERS 
--  Description : Use to auto generate either Ada code
--                       
--  By: Martin Carlisle
--                                         
-- This is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- This is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------

package body Generate_Helpers is


   procedure Generate_Removed_Parameters(File : in Ada.Text_IO.File_Type;
      Action : in String) is
      Start, Parenthesis, Ending : Integer;
   begin
      Start := Action'First;
      Parenthesis := Start-1;
      Ending := Action'Last;
      while Start <= Ending and then Action(Start) = ' ' loop
         Start := Start + 1;
      end loop;

      Ending := Start;
      while Ending <= Action'Last and then
            (Action(Ending) /= ' ' and Action(Ending) /= '(') loop
         Parenthesis := Ending; 
         Ending := Ending + 1;       
      end loop;

      if Parenthesis > Start then
         Ada.Text_IO.Put (File,
            Action(Start..Parenthesis));
      end if;
   end Generate_Removed_Parameters;

   procedure Generate_With(File : in Ada.Text_IO.File_Type;
         Action     : in String) is
      Start, Last_Period, Ending : Integer;
   begin
      Start := Action'First;
      Last_Period := Start-1;
      Ending := Action'Last;
      while Start <= Ending and then Action(Start) = ' ' loop
         Start := Start + 1;
      end loop;

      Ending := Start;
      while Ending <= Action'Last and then
            Action(Ending) /= ' ' and then
            Action(Ending) /= '(' loop
         if Action(Ending) = '.' then
            Last_Period := Ending;
         end if;
         Ending := Ending + 1;
      end loop;

      if Last_Period > Start then
         Ada.Text_IO.Put_Line(File,
            "with " & Action(Start..Last_Period-1) & ";");
      end if;
   end Generate_With;

   -- return name with "." converted to "_"
   function Undot_Name(Name : in String) return String is
      Result : String := Name;
   begin
      for i in Result'range loop
         if Result(i) = '.' or else Result(i) = ' ' then
            Result(i) := '_';
         end if;
      end loop;

      return Result;
   end Undot_Name;

   function Quote_String(Item : in String) return String is
      Result : String(1..2*Item'Length);
      Last   : Natural := 0;
   begin
      for i in Item'range loop
         if Item(i) = '"' then
            Result(Last+1) := '"';
            Result(Last+2) := '"';
            Last := Last + 2;
         else
            Result(Last+1) := Item(i);
            Last := Last + 1;
         end if;
      end loop;
      return Result(1..Last);
   end Quote_String;

end Generate_Helpers;
