-------------------------------------------------------------
-- package base64
--
-- By: Martin C. Carlisle
--     United States Air Force Academy
--     Department of Computer Science
--
-- This file is free software; you can redistribute it and/or 
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
-------------------------------------------------------------
package body base64 is
   type base64_chunk is mod 64;
   
   Translate : array(base64_chunk) of Character :=
      ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
      'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
      'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
      'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
      's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
      '3', '4', '5', '6', '7', '8', '9', '+', '/');

   type base64_block is array(1..4) of base64_chunk;
   
   subtype String3 is String(1..3);
   
   function Convert(x : in String3) return base64_block is
      Result : Base64_Block;
   begin
      -- first 6 bits of x(1)
      Result(1) := base64_chunk(Character'Pos(x(1))/4);
      -- last 2 bits of x(1) + first 4 bits of x(2)
      Result(2) := base64_chunk(
         (Character'Pos(x(1)) rem 4) * 16 +
         (Character'Pos(x(2))/16));
      -- last 4 bits of x(2) and first 2 bits of x(3)
      Result(3) := base64_chunk(
         (Character'Pos(x(2)) rem 16) * 4 +
         (Character'Pos(x(3))/64));
      -- last 6 bits of x(3)
      Result(4) := base64_chunk(
         Character'Pos(x(3)) rem 64);
      return Result;
   end Convert;
   
   ------------
   -- Encode --
   ------------

   -------------------------------------------------------------
   -- The mechanism is that every 24 bits of input go to 32
   -- bits of output.  Actually, each 6 bits go to eight bits, 
   -- but they are processed in groups of four 6 bit blocks
   -- which in the code below is 3 characters.
   -------------------------------------------------------------
   function Encode (Input : in String) return String is
      Block : Base64_Block;
      Extra : Natural := Input'Length rem 3;
      Last_Block : String3 := (others => Character'First);
      Result_Length : Natural;
   begin
      -- make sure we multiply be 4/3 and round up
      -- adding 2 before dividing by 3 accomplishes this with
      -- truncating integer division
      Result_Length := ((Input'Length+2)/3)*4;

      -- pad the last bits with 0's (Character'first is 0)
      if Extra > 0 then
         for i in 1..Extra loop
            Last_Block(i) := Input(Input'Last-Extra+i);
         end loop;
      end if;
      
      declare
         Result : String(1..Result_Length);
      begin
         -- take each 24 bit block and do a translation
         for i in 0..Input'Length/3-1 loop
            Block := Convert(Input(Input'First+i*3..Input'First+i*3+2));
            Result(Result'First+i*4) := 
               Translate(Block(1));
            Result(Result'First+i*4+1) := 
               Translate(Block(2));
            Result(Result'First+i*4+2) := 
               Translate(Block(3));
            Result(Result'First+i*4+3) := 
               Translate(Block(4));
         end loop;
         -- deal with the remainder, if any
         if Extra = 1 then
            Block := Convert(Last_Block);
            Result(Result'Last-3) := Translate(Block(1));
            Result(Result'Last-2) := Translate(Block(2));
            Result(Result'Last-1) := '=';
            Result(Result'Last)   := '=';
         elsif Extra = 2 then
            Block := Convert(Last_Block);
            Result(Result'Last-3) := Translate(Block(1));
            Result(Result'Last-2) := Translate(Block(2));
            Result(Result'Last-1) := Translate(Block(3));
            Result(Result'Last)   := '=';
         end if;

         return Result;      
      end;
   end Encode;

end base64;

