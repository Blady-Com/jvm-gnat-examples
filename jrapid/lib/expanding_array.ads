---------------------------------------------------------------
--                                                           
--  EXPANDING_ARRAY     
--  Description : a lookup table that grows in size as
--                elements are added
--                       
--  By: Martin Carlisle
--                                         
-- EXPANDING_ARRAY is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- EXPANDING_ARRY is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
-- Creates an array that dynamically increases in size starting
-- at Start_Size and doubling every time it is full
---------------------------------------------------------------
generic
   type Element_Type is private;
   Start_Size : Natural;
package Expanding_Array is

   type Expander is private;

   ----------------------------------------------------
   -- procedure Insert
   --
   -- Add an element to the end of the Expanding_Array
   -- and return its index (which can be used to 
   -- retrieve it later)
   ----------------------------------------------------
   procedure Insert(
      Table    : in out Expander;
      Element  : in     Element_Type;
      Location :    out Natural);
      
   ----------------------------------------------------
   -- function Retrieve
   --
   -- return the element at the given location of the
   -- Expanding_Array
   ----------------------------------------------------
   function Retrieve(
      Table    : Expander;
      Location : Natural) return Element_Type;


   ----------------------------------------------------
   -- procedure Replace
   --
   -- Replace the element at the given location
   -- may return Constraint_Error if location doesn't
   -- exist.
   ----------------------------------------------------
   procedure Replace(
      Table    : in out Expander;
      Element  : in     Element_Type;
      Location : in     Natural);
      
private
   type Element_Array is array(Natural range <>) of Element_Type;
   type Element_Array_Pointer is access all Element_Array;
   
   type Expander is record
      Table : Element_Array_Pointer := new Element_Array(1..Start_Size);
      Size  : Natural := 0;
   end record;
end Expanding_Array;  