---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  MENU_IO.ADB
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
-- Change log:
-- 09/20/99 (mcc) : added Separator
---------------------------------------------------------------


with Ada.Integer_Text_IO;
with Ada.Text_IO;
with GUI_Enum;
with File_Helpers;
with GUI,GUI.Menu;
use type GUI_Enum.Keyword;
use type GUI.Menu.Menu_Pointer, GUI.String_Pointer;
with Menu_Edit;

package body Menu_IO is
   subtype Word_String is String(1..80);

   -- This is used so we can generate unique separators even
   -- if the menu is never displayed
   Separator_Number : Natural := 1;

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

   procedure Read_Menubar(File : Ada.Text_IO.File_Type;
         Menubar : out Gui.Menu.Menu_Pointer;
         Count   : in out Integer) is

      function Read_Item(File : Ada.Text_IO.File_Type) return Gui.Menu.Menu_Access is
         Result : Gui.Menu.Menu_Item_Pointer := new Gui.Menu.Menu_Item;
         Name_String   : Word_String;
         Name_Length   : Natural;
         Underline     : Natural;
         Action_String : Word_String;
         Action_Length : Natural;
         Accel_String  : Word_String;
         Accel_Length  : Natural;
      begin
         -- read information
         File_Helpers.Get_String(File => File,
            Item => Name_String, Last => Name_Length);
         Ada.Integer_Text_IO.Get(File,Underline);
         File_Helpers.Get_String(File => File,
            Item => Action_String, Last => Action_Length);
         File_Helpers.Get_String(File => File,
            Item => Accel_String, Last => Accel_Length);
         Ada.Text_IO.Skip_Line(File);

         -- store in record
         --Result.Number := Count;
         Count := Count + 1;

         Result.Name := new String'(Name_String(1..Name_Length));
         Result.Underline_Position := Underline;
         Result.Action := new String'(Action_String(1..Action_Length));
         if Accel_Length > 0 then
            Result.Accelerator := new String'(Accel_String(1..Accel_Length));
         else
            Result.Accelerator := new String'("");
         end if;

         return GUI.Menu.Menu_Access(Result);
      end Read_Item;

      function Read_Menu(File : Ada.Text_IO.File_Type) return Gui.Menu.Menu_Access is
         Result : Gui.Menu.Submenu_Pointer := new Gui.Menu.Submenu;
         Name_String   : Word_String;
         Name_Length   : Natural;
         Underline     : Natural;
         Action_String : Word_String;
         Action_Length : Natural;
      begin
         -- read information
         File_Helpers.Get_String(File => File,
            Item => Name_String, Last => Name_Length);
         Ada.Integer_Text_IO.Get(File,Underline);
         File_Helpers.Get_String(File => File,
            Item => Action_String, Last => Action_Length);
         Ada.Text_IO.Skip_Line(File);

         -- store in record
         --Result.Number := Count;
         Count := Count + 1;

         Result.Name := new String'(Name_String(1..Name_Length));
         Result.Underline_Position := Underline;
         if Action_Length > 0 then
            Result.Action := new String'(Action_String(1..Action_Length));
         else
            Result.Action := NULL;
         end if;

         Read_Menubar(File,Result.Items, Count);

         return GUI.Menu.Menu_Access(Result);
      end Read_Menu;

      Last    : GUI.Menu.Menu_Access;
      Keyword : Gui_Enum.Keyword;
   begin -- Read_Menubar
      Gui.Menu.Menu_List_Package.Initialize(Menubar);
      loop
         Gui_Enum.IO.Get(File,Keyword);

         case Keyword is
            when Gui_Enum.Endof =>
               Gui_Enum.IO.Get(File,Keyword);
               if Keyword = Gui_Enum.Menu or else
                  Keyword = Gui_Enum.Menubar then
                  exit;
               else
                  raise Bad_File;
               end if;
            when Gui_Enum.Item =>
               Last := Read_Item(File);
            when Gui_Enum.Menu =>
               Last := Read_Menu(File);
            when Gui_Enum.Separator =>
               Last := new Gui.Menu.Separator;
               Last.Name := Menu_Edit.Separator_Name'access;
               -- note that separators need to be numbered in
               -- case we generate w/o displaying
               Last.Number := Separator_Number;
               Separator_Number:= Separator_Number + 1;
            when others =>
               raise Bad_File;
         end case;

         Gui.Menu.Menu_List_Package.AddToRear(Menubar,Last);
      end loop;

   exception
      when others => raise Bad_File;
   end Read_Menubar;

   -- writes whole section described above   
   procedure Write_Menubar(File : Ada.Text_IO.File_Type;
         Menubar : Gui.Menu.Menu_Pointer) is
      procedure Write_Submenu(File : Ada.Text_IO.File_Type;
            Submenu : Gui.Menu.Submenu);

      procedure Write_Menu_Item(File : Ada.Text_IO.File_Type;
            Menu_Item : Gui.Menu.Menu_Item) is
      begin
         Gui_Enum.IO.Put(File,Gui_Enum.item);
         Ada.Text_IO.Put(File," """);
         File_Helpers.Put_String(File,Menu_Item.Name.all);
         Ada.Text_IO.Put(File,""" ");
         Ada.Integer_Text_IO.Put(File => File,
            Item  => Menu_Item.Underline_Position,
            Width => 0);
         Ada.Text_IO.Put(File," """);
         File_Helpers.Put_String(File,Menu_Item.Action.all);
         Ada.Text_IO.Put(File,'"');
         if Menu_Item.Accelerator /= NULL then
            Ada.Text_IO.Put(File,' ');
            Ada.Text_IO.Put(File,Menu_Item.Accelerator.all);
         end if;
         Ada.Text_IO.New_Line(File);
      end Write_Menu_Item;

      procedure Write_Menulist(File : Ada.Text_IO.File_Type;
            Menulist : Gui.Menu.Menu_Pointer) is
         Current_Position: Gui.Menu.Menu_Position :=
            Gui.Menu.Menu_List_Package.First(Menulist);
         Current_Menu : Gui.Menu.Menu_Access;
      begin
         while not Gui.Menu.Menu_List_Package.IsPastEnd(
               Menulist,Current_Position) loop
            Current_Menu := Gui.Menu.Menu_List_Package.Retrieve(Menulist,
               Current_Position);
            if Current_Menu.all in GUI.Menu.Submenu'Class then
               Write_Submenu(File,GUI.Menu.Submenu(Current_Menu.all));
            elsif Current_Menu.all in GUI.Menu.Menu_Item'Class then
               Write_Menu_Item(File,GUI.Menu.Menu_Item(Current_Menu.all));
            elsif Current_Menu.all in Gui.Menu.Separator'Class then
               Gui_Enum.IO.Put(File,Gui_Enum.Separator);
               Ada.Text_IO.New_Line(File);
            else
               raise Constraint_Error;
            end if;
            Gui.Menu.Menu_List_Package.GoAhead(
               Menulist,Current_Position);
         end loop;
      end Write_Menulist;

      procedure Write_Submenu(File : Ada.Text_IO.File_Type;
            Submenu : Gui.Menu.Submenu) is
      begin
         Gui_Enum.IO.Put(File,Gui_Enum.menu);
         Ada.Text_IO.Put(File," """);
         File_Helpers.Put_String(File,Submenu.Name.all);
         Ada.Text_IO.Put(File,""" ");
         Ada.Integer_Text_IO.Put(File => File,
            Item  => Submenu.Underline_Position,
            Width => 0);
         if Submenu.Action /= NULL then
            Ada.Text_IO.Put(File," """);
            File_Helpers.Put_String(File,Submenu.Action.all);
            Ada.Text_IO.Put(File,'"');
         end if;
         Ada.Text_IO.New_Line(File);

         Write_Menulist(File,Submenu.Items);

         Gui_Enum.IO.Put(File,Gui_Enum.Endof);
         Ada.Text_IO.Put(File,' ');
         Gui_Enum.IO.Put(File,Gui_Enum.menu);
         Ada.Text_IO.New_Line(File);
      end Write_Submenu;


   begin -- Write_Menubar
      GUI_Enum.IO.Put(File,Gui_Enum.Menubar);
      Ada.Text_IO.New_Line(File);

      Write_Menulist(File,Menubar);

      GUI_Enum.IO.Put(File,Gui_Enum.Endof);
      Ada.Text_IO.Put(File,' ');
      GUI_Enum.IO.Put(File,Gui_Enum.Menubar);
      Ada.Text_IO.New_Line(File);
   end Write_Menubar;

end Menu_IO;