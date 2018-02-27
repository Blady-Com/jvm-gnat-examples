---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  MENU_GENERATE.ADB
--  Description : Generate Ada code for Menu to file
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
-- Change log:
-- 09/20/99 (mcc) : added Separator
-- 08/21/98 (mcc) : changed some Puts to Continuation_Puts to
--                  reduce generated line length
---------------------------------------------------------------
with Gui,Gui.Menu;
use type Gui.Menu.Menu_Pointer,Gui.String_Pointer;
with Mcc.Gui.Menu;
--with Common_Dialogs; -- for debugging

package body Menu_Generate is
   Unrecognized_Type : exception;

   procedure Display_Menus(
         Menubar      : in Gui.Menu.Menu_Pointer;
         Menu         : in out Mcc.Gui.Menu.Menu'Class) is

      Position : Gui.Menu.Menu_Position :=
         Gui.Menu.Menu_List_Package.First(Menubar);
      Traverse : Gui.Menu.Menu_Access;
   begin
      while not Gui.Menu.Menu_List_Package.IsPastEnd(Menubar,Position) loop
         Traverse := Gui.Menu.Menu_List_Package.Retrieve(Menubar,Position);
         if Traverse.all in Gui.Menu.Menu_Item'Class then
            declare
               Item : Mcc.Gui.Menu.Choice;
            begin
               Mcc.Gui.Menu.Add_Choice(
                  Obj         => Item,
                  To_Menu     => Menu,
                  Text        => Traverse.all.Name.all,
                  Action      => null,
                  Underline   => Traverse.all.Underline_Position,
                  Accelerator => Gui.Menu.Menu_Item(
                     Traverse.all).Accelerator.all);
            exception 
               when others =>
                  -- maybe a bad accelerator?
                  null; -- just do nothing
            end;
         elsif Traverse.all in Gui.Menu.Submenu'Class then
            declare
               Submenu : Mcc.Gui.Menu.Submenu;
            begin
               Mcc.Gui.Menu.Add_Submenu(
                  Obj         => Submenu,
                  Text        => Traverse.all.Name.all,
                  Underline   => Traverse.all.Underline_Position,
                  Parent_Menu => Menu);
               Display_Menus(
                  Menubar  => Gui.Menu.Submenu(Traverse.all).Items,
                  Menu     => Submenu);
            end;
         elsif Traverse.all in Gui.Menu.Separator'Class then
            declare
               Separator : Mcc.Gui.Menu.Separator;
            begin
               Mcc.Gui.Menu.Add_Separator(
                  Obj     => Separator,
                  To_Menu => Menu);
            end;
         else
            raise Unrecognized_Type;
         end if;
         Gui.Menu.Menu_List_Package.GoAhead(Menubar,Position);
      end loop;
   end Display_Menus;

   ---------------------------------------------------------------
   -- PROCEDURE Display_Menu_Code
   --
   -- outputs the menu to a window
   ---------------------------------------------------------------
   procedure Display_Menu_Code(
         Menubar   : in     Gui.Menu.Menu_Pointer;
         Window    : in out Gui.Window.GUI_Window;
         Redisplay : in     Boolean := False) is
   begin
      if Redisplay then
         Mcc.Gui.Destroy(Mcc.Gui.Object(Window.Menu_Display));
      end if;
      
      Mcc.Gui.Menu.Create(
         Obj    => Window.Menu_Display,
         Window => Window.Menu_Area);
      if not Gui.Menu.Menu_List_Package.IsEmpty(Menubar) then
         Display_Menus(
            Menubar => Menubar,
            Menu    => Window.Menu_Display);
      else
         declare
            No_Menu : Mcc.Gui.Menu.Submenu;
         begin
            Mcc.Gui.Menu.Add_Submenu(
               Obj         => No_Menu,
               Text        => "No menus",
               Underline   => 0,
               Parent_Menu => Window.Menu_Display);
         end;
      end if;

   end Display_Menu_Code;
end Menu_Generate;