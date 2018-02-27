package body Forks is

   ---------------
   -- Fork_Lock --
   ---------------

   protected body Fork_Lock is
      entry Grap when not Locked is
      begin
         Locked := True;
      end Grap;

      procedure Release is
      begin
         Locked := False;
      end Release;
   end Fork_Lock;

   -----------------
   -- Create_Fork --
   -----------------

   function Create_Fork return Fork_Type is
   begin
      return new Fork_Data;
   end Create_Fork;

   ---------------
   -- Grap_Fork --
   ---------------

   procedure Grap_Fork (Fork : Fork_Type) is
   begin
      Fork.Lock.Grap;
   end Grap_Fork;

   ---------------
   -- Grap_Fork --
   ---------------

   procedure Grap_Fork (Fork1, Fork2 : Fork_Type) is
   begin
      loop
         Fork1.Lock.Grap;
         select
            Fork2.Lock.Grap;
            exit;
         else
            Fork1.Lock.Release;
            Fork2.Lock.Grap;
            select
               Fork1.Lock.Grap;
               exit;
            else
               Fork2.Lock.Release;
            end select;
         end select;
      end loop;
   end Grap_Fork;

   ------------------
   -- Release_Fork --
   ------------------

   procedure Release_Fork (Fork : Fork_Type) is
   begin
      Fork.Lock.Release;
   end Release_Fork;

end Forks;
