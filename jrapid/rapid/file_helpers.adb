---------------------------------------------------------------
--                                                           
--  FILE_HELPERS.ADB 
--  Description : IO helpers
--                       
--  By: Martin Carlisle
--      United States Air Force Academy
--                                         
-- FILE_HELPERS is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- FILE_HELPERS is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
-- Contains two procedures Get_String and Put_String, that
-- allow to read and write all characters inside of " as a single
-- item (may not have ").  If " appears inside, it is doubled.
---------------------------------------------------------------

package body File_Helpers is
   ----------------------------------------------------
   -- PROCEDURE Get_String
   -- 
   -- reads from file the next string.  If first non-blank
   -- is ", then reads until ending ", o/w reads sequence
   -- of non-blank characters.  As in Ada, to embed a string,
   -- use "".  Stops at end of line.
   ----------------------------------------------------

   procedure Get_String(File : in Ada.Text_IO.File_Type;
         Item : out String; Last : out Natural) is
      Next_Char   : Character;
      Is_Quoted   : Boolean;
   begin -- Get_String
      Last := Item'First-1;

      -- skip any preceding blank space
      loop
         if Ada.Text_IO.End_Of_Line(File) then
            return;
         else
            Ada.Text_IO.Get(File,Next_Char);
         end if;

         exit when Next_Char/=' ' and then Next_Char/=ASCII.HT;
      end loop;

      if Next_Char='"' then
         Is_Quoted := True;
         if Ada.Text_IO.End_Of_Line(File) then
            return;
         else
            Ada.Text_IO.Get(File,Next_Char);
         end if;
      else
         Is_Quoted := False;
      end if;

      Last := Last + 1;
      Item(Last) := Next_Char;

      -- get the rest of the string
      loop
         if Is_Quoted then
            if Ada.Text_IO.End_Of_Line(File) then
               return;
            end if;

            Ada.Text_Io.Get(File,Next_Char);
            -- on quote, return if at end of line
            -- or next is not a quote.  If next is a quote
            -- we will only include one of them.
            if Next_Char = '"' then
               if Ada.Text_Io.End_Of_Line(File) then
                  return;
               end if;

               Ada.Text_Io.Get(File,Next_Char);
               if Next_Char /= '"' then
                  return;
               end if;
            end if;
         else
            if Ada.Text_IO.End_Of_Line(File) then
               return;
            end if;

            Ada.Text_Io.Get(File,Next_Char);
            if Next_Char = ' ' or else Next_Char = ASCII.HT then
               return;
            end if;
         end if;

         Last := Last + 1;
         Item(Last) := Next_Char;
      end loop;

   end Get_String;

   ----------------------------------------------------
   -- PROCEDURE Put_String
   -- 
   -- writes string to a file.  If string contains '"' then
   -- two quotation marks are written.
   ----------------------------------------------------
   procedure Put_String(File : in Ada.Text_IO.File_Type;
      Item : in String) is
   begin
      for i in Item'range loop
         if Item(i) = '"' then
            Ada.Text_Io.Put(File => File, Item => """""");
            -- string of two quotation marks
         else
            Ada.Text_Io.Put(File => File, Item => Item(i));
         end if;
      end loop;
   end Put_String;

end File_Helpers;