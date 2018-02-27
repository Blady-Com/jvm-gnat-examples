---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI_MENU.ADS 
--  Description : Implements Menus/Choices
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
-- Contains data types for menus.
--
-- Menu_Access can point to any kind of menu (just one)
--
-- Menu_Pointer points to a list of menus using Feldman's
-- Lists_Generic package
--
-- Menu Hierarchy
--
--             Menu
--            /    \
--      Menu_Item  Submenu
--
-- Submenus are lists of menu items and submenus 
-- (thus contain a Menu_Pointer)
---------------------------------------------------------------
-- Change log:
-- 09/20/99 (mcc) : added separator
---------------------------------------------------------------
with Lists_Generic;
with Ada.Text_IO;

package Gui.Menu is

   type Menu;
   type Menu_Access is access all Menu'Class;

   package Menu_List_Package is new Lists_Generic(ElementType => Menu_Access);
   subtype Menu_Pointer is Menu_List_Package.List;
   subtype Menu_Position is Menu_List_Package.Position;


   -- wbw 6/7/99      
   procedure Generate_Menu_Declaration(Menu : in Menu_Pointer;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99
   procedure Generate_Action_Context_Clause(Menu : in Menu_Pointer;
      File : in Ada.Text_IO.File_Type);

   -- wbw 5/10/99
   procedure Generate_Menu_Creation(Menu : in Menu_Pointer;
      File   : in Ada.Text_IO.File_Type;
      Parent : in String);

   -- all menus can have a name (how displayed)
   -- underline position (accelerator key is Alt+Underlined)
   -- Action (Ada procedure called if this is selected)
   -- Indent_Level (how nested is this?)
   -- 5/9/99
   type Menu is abstract tagged limited record
      Name                 : String_Pointer;
      Underline_Position   : Natural; -- zero based
      Action               : String_Pointer;
      Indent_Level         : Natural; -- used by menu edit
      Number               : Integer;
   end record;

   -- Menu_Items can display an accelerator (e.g. Ctrl+Z)
   type Menu_Item is new Menu with record
      Accelerator          : String_Pointer;
   end record;
   type Menu_Item_Pointer is access all Menu_Item'Class;

   -- A submenu displays a nested menu (list of items, submenus)
   type Submenu is new Menu with record
      Items                : Menu_Pointer;
   end record;
   type Submenu_Pointer is access all Submenu'Class;
   
   -- a separator is just a line across a menu
   type Separator is new Menu with null record;
end Gui.Menu;
