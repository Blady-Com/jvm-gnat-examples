---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET-Progress.ADS
--  Description : GUI Widget Progress bar
--                       
--  By: Martin Carlisle
--                                         
-- RAPID is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- RAPID is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
with Mcc.Gui.Container;
with Mcc.Gui.Widget.Text_Entry;

package Gui.Widget.Progress is

   type Progress is new Gui_Widget with 
      record 
         Fg_Color : String_Pointer := null;  
         Bg_Color : String_Pointer := null;  
         -- Entries
         Fg_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
         Bg_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      end record; 

   -- reads information from file into Progress,
   -- assumes keyword already read.
   procedure Read_Widget (
         Widget : in out Progress;                
         File   : in     Ada.Text_Io.File_Type ); 

   -- Writes information to file from Progress
   procedure Write_Widget (
         Widget : in     Progress;                
         File   : in     Ada.Text_Io.File_Type ); 

   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause (
         Widget : in     Progress;                
         File   : in     Ada.Text_Io.File_Type ); 

   -- wbw 6/6/99
   procedure Generate_Widget_Declaration (
         Widget : in     Progress;                
         File   : in     Ada.Text_Io.File_Type ); 

   -- wbw 5/10/99      
   procedure Generate_Widget_Creation (
         Widget      : in     Progress;                 
         File        : in     Ada.Text_Io.File_Type; 
         Window_Name : in     String                 ); 

   -- display the widget to a window
   procedure Display_Widget (
         Widget    : in out Progress;                            
         Container : in out Mcc.Gui.Container.Container'Class ); 

   procedure Set_Properties (
         Widget : in out Progress ); 

   procedure Apply_Properties (
         Widget : in out Progress ); 
   procedure Check_Properties (
         Widget : in out Progress;  
         Ok     :    out Boolean ); 

end Gui.Widget.Progress;