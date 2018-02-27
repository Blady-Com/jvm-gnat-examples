---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  MENU_IO.ADS 
--  Description : Read/Write Menu to file
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
with Gui.Menu;
with Ada.Text_IO;

package Menu_IO is
   -- assumes the keyword "menubar" has already been read, and
   -- reads the rest of the menu from the file 
   --
   -- This is an LL(1) grammar:
   -- <menubar> -> MENUBAR <menulist> MENUBAR
   -- <menulist> -> MENU <submenuinfo> <menulist> |
   --               ITEM <iteminfo> <menulist> | END
   -- <submenuinfo> -> <name> <underline> <possible_action>
   -- <possible_action> -> <action> | null
   -- <iteminfo> -> <name> <underline> <action> <accelerator>
   -- <accelerator> -> <accel_key> | null
   --
   -- an example of the menubar section of the file:
   -- MENUBAR
   --   MENU File 1 Menu_Action
   --     ITEM New  1 New_Action Ctrl+N
   --     ITEM Open 1 Open_Action Ctrl+O
   --     MENU Submenu 1
   --       ITEM "Sub Item 1" Sub_Action
   --     ENDOF MENU
   --   ENDOF MENU
   --   MENU Edit 1
   --     ITEM Cut 3 Cut_Action 
   --     ITEM Copy 1 Copy_Action
   --   ENDOF MENU
   -- ENDOF MENUBAR

   Bad_File : exception;

   procedure Read_Menubar(File : Ada.Text_IO.File_Type;
      Menubar : out Gui.Menu.Menu_Pointer;
      Count   : in out Integer);

   -- writes whole section described above   
   procedure Write_Menubar(File : Ada.Text_IO.File_Type;
      Menubar : Gui.Menu.Menu_Pointer);

end Menu_IO;