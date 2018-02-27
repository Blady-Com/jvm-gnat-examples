package Forks is

   type Fork_Type is private;
   type Fork_Array is array (Integer range <>) of Fork_Type;

   function Create_Fork return Fork_Type;
   procedure Grap_Fork (Fork : Fork_Type);
   procedure Grap_Fork (Fork1, Fork2 : Fork_Type);
   procedure Release_Fork (Fork : Fork_Type);

private

   protected type Fork_Lock is
      entry Grap;
      procedure Release;
   private
      Locked : Boolean := False;
   end Fork_Lock;

   type Fork_Data is
      record
         Lock : Fork_Lock;
      end record;

   type Fork_Type is access Fork_Data;

end Forks;
