---------------------------------------------------------------
--                                                           
--  GIF_TO_TASH     
--                                                           
--  GIF_TO_TASH.ADB 
--  Description : Converts GIFs to TASH
--                       
--  By: Martin Carlisle
--                                         
-- This work is based on the Free Software Foundation uuencode.c
-- file.  Below is its copyright information:
--/* uuencode utility.
--   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

--   This product is free software; you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation; either version 2, or (at your option)
--   any later version.

--   This product is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU General Public License for more details.

--   You should have received a copy of the GNU General Public License
--   along with this product; see the file COPYING.  If not, write to
--   the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--   02111-1307, USA.  */

--/* Copyright (c) 1983 Regents of the University of California.
--   All rights reserved.

--   Redistribution and use in source and binary forms, with or without
--   modification, are permitted provided that the following conditions
--   are met:
--   1. Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--   2. Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--   3. All advertising materials mentioning features or use of this software
--      must display the following acknowledgement:
--	 This product includes software developed by the University of
--	 California, Berkeley and its contributors.
--   4. Neither the name of the University nor the names of its contributors
--      may be used to endorse or promote products derived from this software
--      without specific prior written permission.

--   THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
--   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--   ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
--   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
--   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
--   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
--   SUCH DAMAGE.  */

--/* Reworked to GNU style by Ian Lance Taylor, ian@airs.com, August 93.  */
--
---------------------------------------------------------------
--with B64_To_Tash_Utils;
with Ada.Text_Io;
with Ada.Sequential_Io;
with ADa.Command_Line;

procedure Gif_To_Tash is
   Line_Length : constant := 57;

   package Character_Sequential_Io is new Ada.Sequential_Io(Character);

   type Mod_Char is mod 2**Character'Size;
   subtype Mod_Array_Indices is Mod_Char range 0..63;

   Translate : array(Mod_Array_Indices) of Character :=
      ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
      'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
      'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
      'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
      's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
      '3', '4', '5', '6', '7', '8', '9', '+', '/');

   -- this is a direct translation of uuencode.c as described above
   procedure Gif_To_B64(Gif_Name : in String;
         B64_Name : in String) is

      function Encode(X : in Mod_Char) return Character is
      begin
         return Translate(X and 8#077#);
      end Encode;

      Gif_File : Character_Sequential_Io.File_Type;
      B64_File : Ada.Text_Io.File_Type;
      Line : String(1..Line_Length);
      N : Natural;
      Ch : Mod_Char;
   begin
      Character_Sequential_Io.Open(File => Gif_File,
         Name => Gif_Name,
         Mode => Character_Sequential_Io.In_File);

      Ada.Text_Io.Create(File => B64_File,
         Name => B64_Name,
         Mode => Ada.Text_Io.Out_File);

      loop
         N := 0;
         -- loop until EOF or have Line_Length characters
         loop
            exit when Character_Sequential_Io.End_Of_File(Gif_File);
            N := N + 1;
            Character_Sequential_Io.Read(File => Gif_File,
               Item => Line(N));
            exit when N = Line_Length;
         end loop;

         exit when N = 0; -- no more in the file!

         for I in 0..(N/3)-1 loop
            Ch := Mod_Char'Val(Character'Pos(Line(3*I+1))) / 4; -- ch = *p >> 2;
            Ada.Text_IO.Put_Line(Line(3*I+1) & ":" &
               mod_char'image(ch) & ":" & encode(ch));
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));

            Ch := ((Mod_Char'Val(Character'Pos(Line(3*I+1))) * 16)
               and 8#060#) or
               ((Mod_Char'Val(Character'Pos(Line(3*I+2))) / 16)
               and 8#017#);
            Ada.Text_IO.Put_Line(
               mod_char'image(ch) & ":" & encode(ch));
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));

            Ch := ((Mod_Char'Val(Character'Pos(Line(3*I+2))) * 4)
               and 8#074#) or
               ((Mod_Char'Val(Character'Pos(Line(3*I+3))) / 64)
               and 8#03#);
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));
            Ada.Text_IO.Put_Line(
               mod_char'image(ch) & ":" & encode(ch));
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));

            Ch := (Mod_Char'Val(Character'Pos(Line(3*I+3))) and 8#077#);
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));
            Ada.Text_IO.Put_Line(Line(3*I+3) & ":" &
               integer'image(Character'Pos(Line(3*I+3))) & ":" &
               mod_char'image(ch) & ":" & encode(ch));
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));
         end loop;

         exit when N mod 3 /= 0;
         -- slightly weird, but shouldn't
         -- happen except on last line          

         Ada.Text_Io.New_Line(B64_File);
      end loop;

      if N mod 3 /= 0 then
         declare
            C1 : Mod_Char := Mod_Char'Val(Character'Pos(
               Line(N - (N mod 3) + 1)));
            C2 : Mod_Char := 0;
            Last : Character;
         begin
            if N mod 3 = 2 then
               C2 := Mod_Char'Val(Character'Pos(Line(N)));
               -- last one for sure
            end if;

            Ch := C1 / 4;
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));

            Ch := ((C1 * 16) and 8#060#) or ((C2 / 16) and 8#017#);
            Ada.Text_Io.Put(File => B64_File, Item => Encode(Ch));

            if N mod 3 = 1 then
               Last := '=';
            else
               Last := Encode((C2 / 4) and 8#074#);
            end if;
            Ada.Text_Io.Put(File => B64_File, Item => Last);

            Ada.Text_Io.Put(File => B64_File, Item => '=');
            Ada.Text_Io.New_Line(B64_File);
         end;
      end if;
      Ada.Text_IO.Close(B64_File);
      Character_Sequential_IO.Close(Gif_File);
   exception
      when Ada.Text_IO.Name_Error => raise;
      when others => raise;
   end Gif_To_B64;

   -- First we create Name.b64 from Name.gif, then
   -- we create name.ad[sb] from name.b64
   begin -- Convert

      Gif_To_B64(Ada.Command_Line.Argument(1) & ".gif", 
         Ada.Command_Line.Argument(1) & ".b64");

      --B64_To_Tash_Utils(Name & ".b64", Name);
   

end Gif_To_Tash;