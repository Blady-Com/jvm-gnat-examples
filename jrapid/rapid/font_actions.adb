---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  FONT_ACTIONS.ADB 
--  Description : Actions while selecting a font
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
with Subwindow_Actions;
with Font_Selection_Dialog_Window;
with Mcc.Gui.Fonts;
with Mcc.Gui.Widget.Listbox;
with Mcc.Gui.Widget.Label;
with Ada.Characters.Handling;
package body Font_Actions is
   Current_Widget : Gui.Widget.Widget_Access;
   type Size_Array is array(Natural range <>) of Mcc.Gui.Fonts.Font_Size;
   Sizes : Size_Array := (
      1  => 8,
      2  => 9,
      3  => 10,
      4  => 11,
      5  => 12,
      6  => 18,
      7  => 20,
      8  => 22,
      9  => 24,
      10 => 26,
      11 => 28,
      12 => 36,
      13 => 48,
      14 => 72);
   
   function Display_Enum(Item : in String) return String is
      Output : String := Ada.Characters.Handling.To_Lower(Item);
      First  : Boolean := True;
   begin
      for i in Output'range loop
         if First then
            Output(i) := Ada.Characters.Handling.To_Upper(Output(i));
            First := False;
         elsif Output(i) = '_' then
            Output(i) := ' ';
            First := True;
         end if;
      end loop;
      
      return Output;
   end Display_Enum;
   
   -------------------
   -- Cancel_Dialog --
   -------------------
   procedure Cancel_Dialog is
   begin
      Mcc.Gui.Container.Window.Destroy(
         Font_Selection_Dialog_Window.Font_Selection_Dialog_Window);
      Subwindow_Actions.Change_Font_Done;
   end Cancel_Dialog;

   -------------------
   -- Cancel_Dialog --
   -------------------
   procedure Cancel_Dialog
     (Obj : in out Mcc.Gui.Widget.Button.Button'Class)
   is
   begin
      Cancel_Dialog;
   end Cancel_Dialog;

   -------------------
   -- Cancel_Dialog --
   -------------------
   procedure Cancel_Dialog
     (Obj : in out Mcc.Gui.Container.Window.Window'Class)
   is
   begin
      Cancel_Dialog;
   end Cancel_Dialog;

   -----------------
   -- Change_Font --
   -----------------

   procedure Change_Font (The_Widget  : in Gui.Widget.Widget_Access) is
   begin
      Current_Widget := The_Widget;
      Font_Selection_Dialog_Window.Generate_Window;
      -- add information to Font_Family list
      for i in reverse Mcc.Gui.Fonts.Font_Family'range loop
         Mcc.Gui.Widget.Listbox.Add_Entry(
            Obj      => Font_Selection_Dialog_Window.Family_Box,
            Location => 0,
            Text     => Display_Enum(Mcc.Gui.Fonts.Font_Family'image(i)));
      end loop;
      
      Mcc.Gui.Widget.Listbox.Select_Item(
         Obj    => Font_Selection_Dialog_Window.Family_Box,
         Number => Mcc.Gui.Fonts.Font_Family'Pos(
            The_Widget.Font_Family)+1);

      -- add information to Font_Style list
      for i in reverse Mcc.Gui.Fonts.Font_Style'range loop
         Mcc.Gui.Widget.Listbox.Add_Entry(
            Obj      => Font_Selection_Dialog_Window.Style_Box,
            Location => 0,
            Text     => Display_Enum(Mcc.Gui.Fonts.Font_Style'image(i)));
      end loop;
      
      Mcc.Gui.Widget.Listbox.Select_Item(
         Obj    => Font_Selection_Dialog_Window.Style_Box,
         Number => Mcc.Gui.Fonts.Font_Style'Pos(
            The_Widget.Font_Style)+1);

      -- add information to Font_Size list
      for i in reverse Sizes'range loop
         Mcc.Gui.Widget.Listbox.Add_Entry(
            Obj      => Font_Selection_Dialog_Window.Size_Box,
            Location => 0,
            Text     => Mcc.Gui.Fonts.Font_Size'image(Sizes(i)));
         
      end loop;
      
      for i in Sizes'range loop
         if The_Widget.Font_Size = Sizes(i) then
            Mcc.Gui.Widget.Listbox.Select_Item(
               Obj    => Font_Selection_Dialog_Window.Size_Box,
               Number => i);
         end if;
      end loop;

      Mcc.Gui.Container.Window.Set_Close_Handler(
         Obj     => 
            Font_Selection_Dialog_Window.Font_Selection_Dialog_Window'access,
         Handler => Cancel_Dialog'access);
   end Change_Font;

   ------------------
   -- Default_Font --
   ------------------

   procedure Default_Font
     (Obj : in out Mcc.Gui.Widget.Button.Button'Class)
   is
   begin
      Current_Widget.Have_Font := False;
      Subwindow_Actions.Change_Font_Done;
      Mcc.Gui.Container.Window.Destroy(
         Font_Selection_Dialog_Window.Font_Selection_Dialog_Window);
      Mcc.Gui.Widget.Label.Set_Text(
         Obj  => Current_Widget.Font_Label.all,
         Text => "Font:");
      Mcc.Gui.Widget.Label.Set_Text(
         Obj  => Current_Widget.Style_Label.all,
         Text => "Default");
   end Default_Font;

   ----------------
   -- Ok_Pressed --
   ----------------

   procedure Ok_Pressed
     (Obj : in out Mcc.Gui.Widget.Button.Button'Class)
   is
   begin
      Current_Widget.Have_Font := True;
      Current_Widget.Font_Size := Sizes(
         Mcc.Gui.Widget.Listbox.Get_Selected(
            Font_Selection_Dialog_Window.Size_Box));
      Current_Widget.Font_Style :=
         Mcc.Gui.Fonts.Font_Style'Val(
            Mcc.Gui.Widget.Listbox.Get_Selected(
               Font_Selection_Dialog_Window.Style_Box)-1);
      Current_Widget.Font_Family :=
         Mcc.Gui.Fonts.Font_Family'Val(
            Mcc.Gui.Widget.Listbox.Get_Selected(
               Font_Selection_Dialog_Window.Family_Box)-1);
      Subwindow_Actions.Change_Font_Done;
      Mcc.Gui.Container.Window.Destroy(
         Font_Selection_Dialog_Window.Font_Selection_Dialog_Window);
      Mcc.Gui.Widget.Label.Set_Text(
         Obj  => Current_Widget.Font_Label.all,
         Text => "Font: " & Display_Enum(
            Mcc.Gui.Fonts.Font_Family'image(Current_Widget.Font_Family)));
      Mcc.Gui.Widget.Label.Set_Text(
         Obj  => Current_Widget.Style_Label.all,
         Text => Display_Enum(
            Mcc.Gui.Fonts.Font_Style'image(Current_Widget.Font_Style)) &
            Mcc.Gui.Fonts.Font_Size'image(Current_Widget.Font_Size) &
            " pt");
   end Ok_Pressed;

end Font_Actions;

