with Ada.Unchecked_Deallocation;
package body Expanding_Array is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Table    : in out Expander;
      Element  : in     Element_Type;
      Location :    out Natural) is
      procedure Free is new Ada.Unchecked_Deallocation(
         Object => Element_Array,
         Name   => Element_Array_Pointer);
      New_Table : Element_Array_Pointer;
   begin
      if Table.Size = Table.Table'Last then
         New_Table := new Element_Array(1..Table.Size*2);
         New_Table(1..Table.Size) := Table.Table(1..Table.Size);
         Free(Table.Table);
         Table.Table := New_Table;
      end if;

      Table.Size := Table.Size + 1;
      Table.Table(Table.Size) := Element;
      Location := Table.Size;
   end Insert;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Table    : in out Expander;
      Element  : in     Element_Type;
      Location : in     Natural)
   is
   begin
      Table.Table(Location) := Element;
   end Replace;

   --------------
   -- Retrieve --
   --------------

   function Retrieve
     (Table    : Expander;
      Location : Natural)
      return Element_Type
   is
   begin
      return Table.Table(Location);
   end Retrieve;

end Expanding_Array;