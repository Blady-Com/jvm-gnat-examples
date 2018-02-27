
package body Util is

   Start : Integer := 1000;

   function Random return Integer is
   begin
      Start := Start - 7;
      return Start;
   end Random;

end util;

