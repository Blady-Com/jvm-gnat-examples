---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-MENU.ADB 
--  Description : Root of GUI MENU Hierarchy
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
-- Change log:
-- 09/21/99 (mcc) : fixed problem with some menu names not
--                  generating legal ada identifiers
---------------------------------------------------------------
with Ada.Characters.Handling;
with Generate_Helpers;

package body Gui.Menu is

   function Convert_Menu_Name(Menu_Name : in String) return String is
      Result  : String := "M" & Menu_Name;
      Start,
      Finish  : Integer;
   begin
      -- make sure we don't begin with non letter
      if Ada.Characters.Handling.Is_Letter(Result(Result'First+1)) then
         Start := Result'First + 1;
      else
         Start := Result'First;
      end if;
      
      for I in Result'range loop
         if (Result(I) = '.' or Result(I) = ' ') or else
            not Ada.Characters.Handling.Is_Alphanumeric(Result(I)) then
            -- need to make sure we don't have two underscores in a row
            if Result(I-1) = '_' then
               Result(I) := 'x';
            else
               Result(I) := '_';
            end if;
         end if;
      end loop;

      -- make sure we don't end in underscore
      Finish := Result'Last;
      while Result(Finish) = '_' loop
         Finish := Finish - 1;
      end loop;
      
      return Result(Start..Finish);
   end Convert_Menu_Name;

   function Separator_Name(Menu : in Menu_Access) return String is
      Result : String := Natural'Image(Menu.Number);
   begin
      return "Separator" & Result(Result'First+1..Result'Last);
   end Separator_Name;

   -- wbw 6/7/99   
   procedure Generate_Menu_Declaration(Menu : in Menu_Pointer;
      File : in Ada.Text_IO.File_Type) is

      Current_Position : Gui.Menu.Menu_Position :=
         Gui.Menu.Menu_List_Package.First(Menu);
      Current_Menu : Gui.Menu.Menu_Access;
   begin
      while not Gui.Menu.Menu_List_Package.IsPastEnd(
            Menu,Current_Position) loop
         Current_Menu := Gui.Menu.Menu_List_Package.Retrieve(Menu,
            Current_Position);

         if Current_Menu.all in GUI.Menu.Submenu'Class then
            Ada.Text_IO.Put_Line(File => File,
               Item => "   " & Convert_Menu_Name(Current_Menu.all.Name.all) & "_SubMenu" &
                  " : aliased Mcc.Gui.Menu.SubMenu;");
            Generate_Menu_Declaration(
               Gui.Menu.Submenu(Current_Menu.all).Items,
               File);
         elsif Current_Menu.all in Gui.Menu.Menu_Item'Class then
            Ada.Text_IO.Put_Line(File => File,
               Item => "   " & Convert_Menu_Name(Current_Menu.all.Name.all) & "_Choice" &
                  " : aliased Mcc.Gui.Menu.Choice;");
         elsif Current_Menu.all in Gui.Menu.Separator'Class then
            Ada.Text_IO.Put_Line(File => File,
               Item => "   " & Separator_Name(Current_Menu) & 
                  " : aliased Mcc.Gui.Menu.Separator;");
         else
            raise Constraint_Error;
         end if;
         Gui.Menu.Menu_List_Package.GoAhead(
            Menu,Current_Position);
         end loop;
   end Generate_Menu_Declaration;

   procedure Generate_Action_Context_Clause(Menu : in Menu_Pointer;
      File : in Ada.Text_IO.File_Type) is
      Current_Menu_Position : Gui.Menu.Menu_Position :=
         Gui.Menu.Menu_List_Package.First(Menu);
      Current_Menu          : Gui.Menu.Menu_Access;
   begin
      while not Gui.Menu.Menu_List_Package.IsPastEnd(L => Menu,
         P => Current_Menu_Position) loop
         Current_Menu := Gui.Menu.Menu_List_Package.Retrieve(L => Menu,
            P => Current_Menu_Position);
         if Current_Menu.all in Menu_Item'Class then
            Generate_Helpers.Generate_With(File,Current_Menu.Action.all);
         end if;
         if Current_Menu.all in SubMenu'Class then
            Generate_Action_Context_Clause(Menu => SubMenu(Current_Menu.all).Items,
               File => File);
         end if;
         Gui.Menu.Menu_List_Package.GoAhead(L => Menu, P => Current_Menu_Position);
      end loop;
   end Generate_Action_Context_Clause;

   procedure Generate_Menu_Creation(Menu : in Menu_Pointer;
      File   : in Ada.Text_IO.File_Type;
      Parent : in String) is
      Current_Position : Gui.Menu.Menu_Position :=
         Gui.Menu.Menu_List_Package.First(Menu);
      Current_Menu : Gui.Menu.Menu_Access;
   begin
      while not Gui.Menu.Menu_List_Package.IsPastEnd(
            Menu,Current_Position) loop
         Current_Menu := Gui.Menu.Menu_List_Package.Retrieve(Menu,
            Current_Position);

         if Current_Menu.all in GUI.Menu.Submenu'Class then
            Ada.Text_IO.Put_Line(File => File,
               Item => "      Mcc.Gui.Menu.Add_SubMenu(");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Obj         => " & Convert_Menu_Name(Current_Menu.all.Name.all) &
                       "_SubMenu" & ",");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Text        => " & """" & Current_Menu.all.Name.all & """,");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Underline   =>" & Integer'Image(Current_Menu.all.Underline_Position) & ",");
            Ada.Text_IO.Put(File => File,
               Item => "         Parent_Menu => " & Parent);
            if Current_Menu.all.Action /= null and then Current_Menu.all.Action.all /= "null" then
               Ada.Text_IO.Put_Line(File => File,
                  Item => ",");
               Ada.Text_IO.Put_Line(File => File,
                  Item => "      On_Post     => " & 
                     Current_Menu.all.Action.all & "'access" & ");");
            else
               Ada.Text_IO.Put_Line(File => File,
                  Item => ");");
            end if;
            Generate_Menu_Creation(
               Menu   => Gui.Menu.Submenu(Current_Menu.all).Items,
               File   => File,
               Parent => Convert_Menu_Name(Current_Menu.all.Name.all) & "_SubMenu");
         elsif Current_Menu.all in GUI.Menu.Menu_Item'Class then
            Ada.Text_IO.Put_Line(File => File,
               Item => "      Mcc.Gui.Menu.Add_Choice(");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Obj         => " & Convert_Menu_Name(Current_Menu.all.Name.all) &
                       "_Choice" & ",");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         To_Menu     => " & Parent & ",");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Text        => " & """" & Current_Menu.all.Name.all & """,");
            -- although action is an attribute of menu, this will change in the future,
            -- so menu_item is used here
            if Ada.Characters.Handling.To_Lower(Gui.Menu.Menu_Item(Current_Menu.all).Action.all)
               /= "null" then
               Ada.Text_IO.Put_Line(File => File,
                  Item => "         Action      => " & Current_Menu.all.Action.all & "'access" & ",");               
            else
               Ada.Text_IO.Put_Line(File => File,
                  Item => "         Action      => null,");
            end if;
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Underline   =>" & Integer'Image(
                  Current_Menu.all.Underline_Position) & ",");
            if Gui.Menu.Menu_Item(Current_Menu.all).Accelerator /= null then
               Ada.Text_IO.Put_Line(File => File,
                  Item => "         Accelerator => "
                     & """" & Gui.Menu.Menu_Item(Current_Menu.all).Accelerator.all & """" & ");");
            else
               Ada.Text_IO.Put_Line(File => File,
                  Item => "         Accelerator => """");");
            end if;
         elsif Current_Menu.all in GUI.Menu.Separator'Class then
            Ada.Text_IO.Put_Line(File => File,
               Item => "      Mcc.Gui.Menu.Add_Separator(");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         Obj         => " & 
                  Separator_Name(Current_Menu) & ",");
            Ada.Text_IO.Put_Line(File => File,
               Item => "         To_Menu     => " & Parent & ");");
         else
            raise Constraint_Error;
         end if;
         Gui.Menu.Menu_List_Package.GoAhead(
            Menu,Current_Position);
         end loop;
   end Generate_Menu_Creation;

end Gui.Menu;