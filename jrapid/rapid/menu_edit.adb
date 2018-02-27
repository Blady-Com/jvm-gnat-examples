---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  MENU_EDIT.ADB
--  Description : Used to visually edit the menus
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
with Menuedit_Window;
with Gui,Gui.Menu,Gui.Window;
use type Gui.String_Pointer;
with State;
with Ada.Unchecked_Deallocation;
with Submenu_Dialog_Window,Menuitem_Dialog_Window;
with Menu_Generate;
with Mcc.Gui.Widget.Text_Entry;
with Mcc.Gui.Widget.Listbox;
with Mcc.Gui.Widget.Button.Radio;
use type Mcc.Gui.Widget.Button.Radio.Radio_Pointer;
with Mcc.Common_Dialogs; -- for debugging

package body Menu_Edit is
   Indent_Width  : constant := 20;
   Item_Height   : constant := 15;
   Canvas_Width  : constant := 269;
   Insert_Inside : Boolean := False;
   
   Number_Entries : Integer;
   Selected_Item  : Integer;
   Selected_Menu  : Gui.Menu.Menu_Access;
   type Item_Type is (Menu,Item,Separator);
   Am_Inserting   : Boolean := False;
   Active_Type    : Item_Type;
   -- we cannot allow two subdialogs to run simultaneously
   Subdialog_Running : Boolean := False;

   type Operation_Type is (Insert,Delete,Edit);

   procedure Free is new Ada.Unchecked_Deallocation(String,
      Gui.String_Pointer);
   -----------------------------------------------------------------
   -- procedure Find_Selected_Item
   -- 
   -- Look for the appropriate number in the menubar
   -- return if it is found, its position, and perform an operation
   -- (either insert, delete, or edit)
   --
   -- Walk through the list until you find the number 
   -- On finding a submenu, recursively call this procedure
   -- 
   -- If we find the number, delete it, or set the 
   -- package variable Selected_Menu (for edit),
   -- or insert something in the list.  For insert, check if
   -- highlighted is a submenu.  If so, determine where to insert
   -- based on that submenus Insert_Inside setting.
   -----------------------------------------------------------------
   procedure Find_Selected_Item(Seeking : in Integer;
         Menubar   : in out Gui.Menu.Menu_Pointer;
         Position  : out Gui.Menu.Menu_Position;
         Operation : in Operation_Type;
         Found     : out Boolean) is
      Current_Position: Gui.Menu.Menu_Position :=
         Gui.Menu.Menu_List_Package.First(Menubar);
      Current_Menu : Gui.Menu.Menu_Access;
   begin
      while not Gui.Menu.Menu_List_Package.IsPastEnd(
            Menubar,Current_Position) loop
         Current_Menu := Gui.Menu.Menu_List_Package.Retrieve(Menubar,
            Current_Position);
         if Current_Menu.Number = Seeking then
            Position := Current_Position;
            Found    := True;
            case Operation is
               when Insert =>
                  declare
                     Before_Menu : Gui.Menu.Menu_Access;
                  begin
                     Before_Menu := Gui.Menu.Menu_List_Package.Retrieve(
                        Menubar,Position);
                     if Before_Menu.all in Gui.Menu.Submenu'Class and then
                           Insert_Inside then
                        Gui.Menu.Menu_List_Package.AddToFront(
                           Gui.Menu.Submenu(Before_Menu.all).Items,
                           Selected_Menu);
                     else
                        Gui.Menu.Menu_List_Package.Insert(Menubar,
                           Selected_Menu,Position);
                     end if;
                  end;
               when Delete =>
                  Gui.Menu.Menu_List_Package.Delete(Menubar,Position);
               when Edit =>
                  Selected_Menu := Gui.Menu.Menu_List_Package.Retrieve(
                     Menubar,Position);
            end case;
            return;
         elsif Current_Menu.all in GUI.Menu.Submenu'Class then
            Find_Selected_Item(
               Seeking   => Seeking,
               Menubar   => Gui.Menu.Submenu(Current_Menu.all).Items,
               Operation => Operation,
               Position  => Position,
               Found     => Found);
            if Found then
               return;
            end if;
         end if;
         Gui.Menu.Menu_List_Package.GoAhead(
            Menubar,Current_Position);
      end loop;
      Found := False;
   end Find_Selected_Item;


   -----------------------------------------------------------------
   -- display submenu or menu item
   -- 
   -----------------------------------------------------------------
   procedure Display_Item(
         Menu         : in Gui.Menu.Menu_Access;
         Indent_Level : in Integer;
         Number       : in out Integer;
         Cascade      : in Boolean) is
         Postfix : Character;
   begin
      if Cascade then
         Postfix := '*';
      else
         Postfix := ' ';
      end if;
      
      Menu.Number       := Number;
      Menu.Indent_Level := Indent_Level;

      declare
         Indent : String(1..Indent_Level*3) := (others => ' ');
      begin
         Mcc.Gui.Widget.Listbox.Add_Entry(
            Obj      => Menuedit_Window.Menu_Display,
            Location => Number,
            Text     => Indent & Menu.Name.all & Postfix);
      end;
         
      if Menu.Number > Number_Entries then
         Number_Entries := Menu.Number;
      end if;

      Number := Number + 1;
   end Display_Item;

   -----------------------------------------------------------------
   -- Display menus in edit window
   -- 
   -- Traverse list calling Display_Item
   -- recursively call this adding 1 to indent level
   -- for submenus
   -----------------------------------------------------------------
   procedure Display_Menus(
         Menubar      : in Gui.Menu.Menu_Pointer;
         Indent_Level : in Integer := 1;
         Number       : in out Integer) is

      Position : Gui.Menu.Menu_Position :=
         Gui.Menu.Menu_List_Package.First(Menubar);
      Traverse : Gui.Menu.Menu_Access;
   begin
      if Indent_Level = 1 then
         Mcc.Gui.Widget.Listbox.Add_Entry(
            Obj      => Menuedit_Window.Menu_Display,
            Location => 0,
            Text     => "MENUBAR");
      end if;
      
      while not Gui.Menu.Menu_List_Package.IsPastEnd(Menubar,Position) loop
         Traverse := Gui.Menu.Menu_List_Package.Retrieve(Menubar,Position);
         if Traverse.all in Gui.Menu.Menu_Item'Class or else
            Traverse.all in Gui.Menu.Separator'Class then
            Display_Item(
               Menu         => Traverse,
               Indent_Level => Indent_Level,
               Number       => Number,
               Cascade      => False);
         elsif Traverse.all in Gui.Menu.Submenu'Class then
            Display_Item(
               Menu         => Traverse,
               Indent_Level => Indent_Level,
               Number       => Number,
               Cascade      => True);
            Display_Menus(
               Menubar      => Gui.Menu.Submenu(Traverse.all).Items,
               Indent_Level => Indent_Level + 1,
               Number       => Number);
         else
            raise Constraint_Error;
         end if;
         Gui.Menu.Menu_List_Package.GoAhead(Menubar,Position);
      end loop;
   end Display_Menus;

   procedure Menu_Display_Mouse_Listener(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
      use Mcc.Gui;
   begin
      if (Event.Button = Left and Event.Action = Double_Click) or
         (Event.Button = Right and Event.Action = Press) then
         Edit_Item;
      end if;
   end Menu_Display_Mouse_Listener;
   -----------------------------------------------------------------
   -- pops up a window that Edits the menu
   --
   -- 1) Generate window 
   -- 2) Select a radio
   -- 3) bind close and mouse
   -----------------------------------------------------------------
   procedure Edit(Menubar : in Gui.Menu.Menu_Pointer;
         Window  : in Mcc.Gui.Container.Container'Class) is
      Number    : Integer;
   begin -- Edit
      State.Set_Dialog_Running(True);
      Menuedit_Window.Generate_Window;
      
      Mcc.Gui.Container.Window.Set_Close_Handler(
         Obj     => Menuedit_Window.Menuedit_Window'access,
         Handler => Done_Edit'access);

      
      Number_Entries := 0;
      Selected_Item  := 0;
      Number         := 1;

      Display_Menus(
         Menubar      => Menubar,
         Number       => Number);
      
      -- select a position to insert
      Mcc.Gui.Widget.Button.Radio.Select_Radio(
         Menuedit_Window.Inside);
         
      -- bind double click in Menu_Display to edit_Item?
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Menuedit_Window.Menu_Display'access,
         Listener => Menu_Display_Mouse_Listener'access);
   end Edit;

   -----------------------------------------------------------------
   -- procedure Insert_Menu
   --
   -- get selected item
   -- set insert_inside
   -- set am_inserting
   -- set active_type
   -- set subdialog_running
   -- run submenu dialog
   -- give focus to entry 1
   -- set window close to cancel
   -----------------------------------------------------------------
   procedure Insert_Menu is
   begin
      if Subdialog_Running then
         return;
      end if;
      -- offset for MENUBAR
      Selected_Item := Mcc.Gui.Widget.Listbox.Get_Selected(
         Menuedit_Window.Menu_Display)-1;
         
      Insert_Inside := (Mcc.Gui.Widget.Button.Radio.Get_Selected(
         Menuedit_Window.Insert_Group) = Menuedit_Window.Inside'access);
      
      Am_Inserting := True;
      Active_Type  := Menu;
      Subdialog_Running := True;
      Submenu_Dialog_Window.Generate_Window;
      Mcc.Gui.Widget.Text_Entry.Highlight(
         Obj   => Submenu_Dialog_Window.entry1,
         Start => 0,
         Stop  => 0);
      Mcc.Gui.Container.Window.Set_Close_Handler(
         Obj     => Submenu_Dialog_Window.Submenu_Dialog_Window'access,
         Handler => Cancel_Insert_Dialog'access);
   end Insert_Menu;
   procedure Insert_Menu(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Insert_Menu;
   end Insert_Menu;

   -----------------------------------------------------------------
   -- procedure Insert_Menu
   --
   -- get selected item
   -- set insert_inside
   -- set am_inserting
   -- set active_type
   -- set subdialog_running
   -- run menuitem dialog
   -- give focus to entry 1
   -- set window close to cancel
   -----------------------------------------------------------------
   procedure Insert_Choice is
   begin
      if Subdialog_Running then
         return;
      end if;
      -- offset for MENUBAR
      Selected_Item := Mcc.Gui.Widget.Listbox.Get_Selected(
         Menuedit_Window.Menu_Display)-1;
      
      Insert_Inside := (Mcc.Gui.Widget.Button.Radio.Get_Selected(
         Menuedit_Window.Insert_Group) = Menuedit_Window.Inside'access);
      
      Am_Inserting := True;
      Active_Type  := Item;
      Subdialog_Running := True;
      Menuitem_Dialog_Window.Generate_Window;
      Mcc.Gui.Widget.Text_Entry.Highlight(
         Obj   => Menuitem_Dialog_Window.entry1,
         Start => 0,
         Stop  => 0);
      Mcc.Gui.Container.Window.Set_Close_Handler(
         Obj     => Menuitem_Dialog_Window.Menuitem_Dialog_Window'access,
         Handler => Cancel_Insert_Dialog'access);
   end Insert_Choice;
   procedure Insert_Choice(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Insert_Choice;
   end Insert_Choice;


   procedure Insert_Separator(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      if Subdialog_Running then
         return;
      end if;
      -- offset for MENUBAR
      Selected_Item := Mcc.Gui.Widget.Listbox.Get_Selected(
         Menuedit_Window.Menu_Display)-1;
      
      Insert_Inside := (Mcc.Gui.Widget.Button.Radio.Get_Selected(
         Menuedit_Window.Insert_Group) = Menuedit_Window.Inside'access);
      
      Am_Inserting := True;
      Active_Type  := Separator;
      Done_Insert_Dialog;
   end Insert_Separator;
   -----------------------------------------------------------------
   -- procedure Redraw_Display
   -- 
   -- delete everything from canvas and redraw them all
   -- not exactly efficient, but it works
   -----------------------------------------------------------------
   procedure Redraw_Display is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Number      : Integer;
   begin
      Mcc.Gui.Widget.Listbox.Clear(Menuedit_Window.Menu_Display);

      Number         := 1;

      Display_Menus(
         Menubar      => Window.Menu,
         Number       => Number);
      if Selected_Item > Number_Entries then
         Selected_Item := Number_Entries;
      end if;
      -- offset for MENUBAR
      Mcc.Gui.Widget.Listbox.Select_Item(
         Obj    => Menuedit_Window.Menu_Display,
         Number => Selected_Item+1);
   end Redraw_Display;

   -----------------------------------------------------------------
   -- procedure Delete
   --
   -- call Find_Selected_Item to do work for you
   -- Redraw display if something was found
   -----------------------------------------------------------------
   procedure Delete is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Item        : Gui.Menu.Menu_Position;
      Found       : Boolean;
   begin
      if Subdialog_Running then
         return;
      end if;

      Selected_Item := Mcc.Gui.Widget.Listbox.Get_Selected(
         Menuedit_Window.Menu_Display);
      
      if Selected_Item = 0 then
         return;
      else
         -- offset for MENUBAR
         Selected_Item := Selected_Item - 1;
      end if;

      Find_Selected_Item(Seeking => Selected_Item,
         Menubar  => Window.Menu,
         Position => Item,
         Operation=> Delete,
         Found    => Found);

      if Found then
         Redraw_Display;
      end if;

      State.Set_Changed(True);
   end Delete;
   procedure Delete(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Delete;
   end Delete;

   -----------------------------------------------------------------
   -- procedure Edit_Item
   --
   -- run Find_Selected_Item
   -- then performs code similar to insert for appropriate
   -- type of menu (submenu or item), also using Common_Entries
   -- to set name, underline and action text entries.
   -----------------------------------------------------------------
   procedure Edit_Item is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      This_Item   : Gui.Menu.Menu_Position;
      Found       : Boolean;
   begin
      if Subdialog_Running then
         return;
      end if;
      
      Selected_Item := Mcc.Gui.Widget.Listbox.Get_Selected(
         Menuedit_Window.Menu_Display);
      
      if Selected_Item = 0 then
         return;
      else
         -- offset for MENUBAR
         Selected_Item := Selected_Item - 1;
      end if;
   
      Find_Selected_Item(Seeking => Selected_Item,
         Menubar  => Window.Menu,
         Position => This_Item,
         Operation=> Edit,
         Found    => Found);

      Am_Inserting := False;

      if Found then
         if Selected_Menu.all in Gui.Menu.Submenu'Class then
            Active_Type := Menu;
            Subdialog_Running := True;
            Submenu_Dialog_Window.Generate_Window;
            Mcc.Gui.Widget.Text_Entry.Highlight(
               Obj   => Submenu_Dialog_Window.Entry1,
               Start => 0,
               Stop  => 0);
            Mcc.Gui.Container.Window.Set_Close_Handler(
               Obj     => Submenu_Dialog_Window.Submenu_Dialog_Window'access,
               Handler => Cancel_Insert_Dialog'access);
            Mcc.Gui.Widget.Text_Entry.Set_Text(
               Obj  => Submenu_Dialog_Window.Entry1,
               Text => Selected_Menu.Name.all);
            Mcc.Gui.Widget.Text_Entry.Set_Text(
               Obj  => Submenu_Dialog_Window.Entry2,
               Text => Integer'image(Selected_Menu.Underline_Position));
            Mcc.Gui.Widget.Text_Entry.Set_Text(
               Obj  => Submenu_Dialog_Window.Entry3,
               Text => Selected_Menu.Action.all);
         elsif Selected_Menu.all in Gui.Menu.Menu_Item'Class then
            Active_Type := Item;
            Subdialog_Running := True;
            Menuitem_Dialog_Window.Generate_Window;
            Mcc.Gui.Container.Window.Set_Close_Handler(
               Obj     => Menuitem_Dialog_Window.Menuitem_Dialog_Window'access,
               Handler => Cancel_Insert_Dialog'access);
            Mcc.Gui.Widget.Text_Entry.Set_Text(
               Obj  => Menuitem_Dialog_Window.Entry1,
               Text => Selected_Menu.Name.all);
            Mcc.Gui.Widget.Text_Entry.Set_Text(
               Obj  => Menuitem_Dialog_Window.Entry2,
               Text => Integer'image(Selected_Menu.Underline_Position));
            Mcc.Gui.Widget.Text_Entry.Set_Text(
               Obj  => Menuitem_Dialog_Window.Entry3,
               Text => Selected_Menu.Action.all);
            if Gui.Menu.Menu_Item(Selected_Menu.all).Accelerator /= null then
               Mcc.Gui.Widget.Text_Entry.Set_Text(
                  Obj  => Menuitem_Dialog_Window.Entry4,
                  Text => Gui.Menu.Menu_Item(
                     Selected_Menu.all).Accelerator.all);
            end if;
         elsif Selected_Menu.all in Gui.Menu.Separator'Class then
            null; -- can't edit a separator
         else
            raise Constraint_Error;
         end if;
      end if;
   end Edit_Item;
   procedure Edit_Item(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Edit_Item;
   end Edit_Item;

   -----------------------------------------------------------------
   -- procedure Done_Insert_Dialog
   --
   -- keeps track of whether inserting vs editing (same dialog is used)
   -----------------------------------------------------------------
   procedure Done_Insert_Dialog is
      Number  : Integer;
      Cascade : Boolean;
      Found   : Boolean;
      Window  : Gui.Window.Window_Pointer := State.Get_Current_Window;
      This_Item    : Gui.Menu.Menu_Position;
   begin
      if Am_Inserting then
         if Active_Type = Menu then
            Selected_Menu := new Gui.Menu.Submenu;
         elsif Active_Type = Item then
            Selected_Menu := new Gui.Menu.Menu_Item;
         elsif Active_Type = Separator then
            Selected_Menu := new Gui.Menu.Separator;
         end if;

         if Selected_Item > 0 then
            Find_Selected_Item(Seeking => Selected_Item,
               Menubar  => Window.Menu,
               Position => This_Item,
               Operation=> Insert,
               Found    => Found);
         else
            Found := True;
            Gui.Menu.Menu_List_Package.AddToFront(Window.Menu,
               Selected_Menu);
         end if;

         if not Found then
            return;
         end if;

      end if;

      Free(Selected_Menu.Name);
      Free(Selected_Menu.Action);
      if Selected_Menu.all in Gui.Menu.Submenu'Class then
         Cascade := True;
         declare
            Name : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
               Obj => Submenu_Dialog_Window.Entry1);
         begin
            if Name'Length > 0 then
               Selected_Menu.Name := new String'(Name);
            else
               Selected_Menu.Name := new String'("Untitled");
            end if;
         end;
         begin
            Selected_Menu.Underline_Position := 
               Mcc.Gui.Widget.Text_Entry.Get_Text(
                  Obj => Submenu_Dialog_Window.Entry2);
         exception when others =>
            Selected_Menu.Underline_Position := 0;
         end;
         declare
            Action : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
               Obj => Submenu_Dialog_Window.Entry3);
         begin
            if Action'Length > 0 then
               Selected_Menu.Action := new String'(Action);
            else
               Selected_Menu.Action := new String'("null");
            end if;
         end;
         Cancel_Insert_Dialog;
      elsif Selected_Menu.all in Gui.Menu.Menu_Item'Class then
         Cascade := False;
         Free(Gui.Menu.Menu_Item(Selected_Menu.all).Accelerator);
         declare
            Name : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
               Obj => Menuitem_Dialog_Window.Entry1);
         begin
            if Name'Length > 0 then
               Selected_Menu.Name := new String'(Name);
            else
               Selected_Menu.Name := new String'("Untitled");
            end if;
         end;
         begin
            Selected_Menu.Underline_Position := 
               Mcc.Gui.Widget.Text_Entry.Get_Text(
                  Obj => Menuitem_Dialog_Window.Entry2);
         exception when others =>
            Selected_Menu.Underline_Position := 0;
         end;
         declare
            Action : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
               Obj => Menuitem_Dialog_Window.Entry3);
         begin
            if Action'Length > 0 then
               Selected_Menu.Action := new String'(Action);
            else
               Selected_Menu.Action := new String'("null");
            end if;
         end;
         declare
            Accelerator : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
               Obj => Menuitem_Dialog_Window.Entry4);
         begin
            Gui.Menu.Menu_Item(Selected_Menu.all).Accelerator :=
               new String'(Accelerator);
         end;
         Cancel_Insert_Dialog;
      elsif Selected_Menu.all in Gui.Menu.Separator'Class then
         Selected_Menu.Name := Separator_Name'access;
      else
         raise Constraint_Error;
      end if;

      if not Am_Inserting then
         Mcc.Gui.Widget.Listbox.Delete_Entry(
            Obj   => Menuedit_Window.Menu_Display,
            Start => Selected_Menu.Number+1,
            Stop  => Selected_Menu.Number+1);

         -- since Number is in out, use a copy
         Number := Selected_Menu.Number;
         Display_Item(
            Menu         => Selected_Menu,
            Indent_Level => Selected_Menu.Indent_Level,
            Number       => Number,
            Cascade      => Cascade);
         Mcc.Gui.Widget.Listbox.Select_Item(
            Obj    => Menuedit_Window.Menu_Display,
            Number => Number);
      else
         Redraw_Display;
         -- move down 1 (see Redraw_Display)
         Mcc.Gui.Widget.Listbox.Select_Item(
            Obj    => Menuedit_Window.Menu_Display,
            Number => Selected_Item+2);
      end if;


      State.Set_Changed(True);
   end Done_Insert_Dialog;

   -----------------------------------------------------------------
   -- procedure Cancel_Insert_Dialog
   --
   -- destroy the dialog
   -----------------------------------------------------------------
   procedure Cancel_Insert_Dialog is
   begin
      if Active_Type = Menu then
         Mcc.Gui.Container.Window.Destroy(
            Submenu_Dialog_Window.Submenu_Dialog_Window);
      else
         Mcc.Gui.Container.Window.Destroy(
            Menuitem_Dialog_Window.Menuitem_Dialog_Window);
      end if;
      Subdialog_Running := False;
   end Cancel_Insert_Dialog;
   procedure Cancel_Insert_Dialog(
      Obj : in out Mcc.Gui.Container.Window.Window'Class) is
   begin
      Cancel_Insert_Dialog;
   end Cancel_Insert_Dialog;

   procedure Cancel_Insert_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Cancel_Insert_Dialog;
   end Cancel_Insert_Dialog;

   procedure Done_Edit is
      Window       : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Was_Changed  : Boolean;
   begin
      if Subdialog_Running then
         return;
      end if;

      Mcc.Gui.Container.Window.Destroy(
         Menuedit_Window.Menuedit_Window);
      State.Set_Dialog_Running(False);
      -- Ugh!  Doing this resizes the window, so 
      -- I remember the window size above, and
      -- reinstate it after redisplaying the menu
      -- even worse, this resize changes the state to true
      Was_Changed := State.Get_Changed;
      Menu_Generate.Display_Menu_Code(
         Menubar     => Window.Menu,
         Window      => Window.all,
         Redisplay   => True);

      State.Set_Changed(Was_Changed);
   end Done_Edit;
   
   procedure Done_Edit(
      Obj : in out Mcc.Gui.Container.Window.Window'Class) is
   begin
      Done_Edit;
   end Done_Edit;
   
   procedure Done_Edit(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Done_Edit;
   end Done_Edit;
   
   procedure Done_Insert_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Done_Insert_Dialog;
   end Done_Insert_Dialog;
   

end Menu_Edit;