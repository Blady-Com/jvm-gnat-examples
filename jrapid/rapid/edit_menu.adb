---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  EDIT_MENU.ADB
--  Description : Implements choices from the Edit menu
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
with Gui;
use type Gui.String_Pointer;
with Gui.Widget,Gui.Window;
use type Gui.Widget.Widget_Access,Gui.Window.Window_Pointer;
with State;
with Subwindow_Actions;
with Change_Window_Dialog_Window;
with Mcc.Gui.Widget.Text_Entry;
--with Common_Dialogs; -- for debugging

package body Edit_Menu is
   -------------------------------------------------------------
   -- procedure Change_Window
   --
   -- used to change window's name, width or height
   --
   -- Algorithm
   -- 1) Find current window
   -- 2) Generate change dialog
   --    dialog (change_window_dialog) has 3 entries 
   --      entry1: name
   --      entry2: width
   --      entry3: height
   --      entry4: title (added 2/22/99 using suggestion by Dmitri 
   --                     Anisimkov)
   -- 3) Make change dialog transient (changes appearance) 
   -- 4) Main window is stored as ".", but display as "main"
   --    Set text in entry1, 2 and 3
   -- 5) Callbacks implement changes, so we are done!
   -------------------------------------------------------------
   procedure Change_Window is
      Window    : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      Change_Window_Dialog_Window.Generate_Window;
      Mcc.Gui.Widget.Text_Entry.Highlight(
         Obj   => Change_Window_Dialog_Window.Entry1,
         Start => 0,
         Stop  => 0);

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Change_Window_Dialog_Window.Entry1,
         Text =>
            Window.Window_Name.all(
              Window.Window_Name.all'First..
              Window.Window_Name.all'Last));

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Change_Window_Dialog_Window.Entry2,
         Text => Integer'Image(Window.Width));

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Change_Window_Dialog_Window.Entry3,
         Text => Integer'Image(Window.Height));

      if Window.Title/=null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Change_Window_Dialog_Window.Entry4,
            Text => Window.Title.all);
      end if;

   end Change_Window;

   -------------------------------------------------------------
   -- function Check_Changes
   --
   -- verify that entries have legal values
   -- highlight illegal entry (if it is negative)
   -------------------------------------------------------------
   function Check_Changes(Window : in Gui.Window.Window_Pointer)
         return Boolean is
   begin
      if Window.Width < 0 then
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Obj   => Change_Window_Dialog_Window.Entry2);
         return false;
      elsif Window.Height < 0 then
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Obj   => Change_Window_Dialog_Window.Entry3);
         return false;
      else
         return true;
      end if;
   end Check_Changes;

   -------------------------------------------------------------
   -- procedure Set_Changes/Set_Name
   --
   -- put values from entries in Window record
   --
   -- These used to be one procedure, but we don't want to reset
   -- name until after we have undisplayed the old window
   --
   -- Algorithm
   -- Set_Name
   -- 1) Read entry1 into Word
   -- 2) Put Word into Window.Name, replacing "." with "_"
   --    (Ada doesn't like . in its names)
   -- Set_Changes
   -- 3) Read entry2 directly into Window.Width
   -- 4) Read entry3 directly into Window.Height
   -- 5) Read entry4 into Word, then into Window.Title (added 2/22/99)
   -------------------------------------------------------------
   procedure Set_Name(Window : in Gui.Window.Window_Pointer) is
      Word : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
         Change_Window_Dialog_Window.Entry1);
   begin
      
      if Word'Last > Word'First then
         Window.Window_Name := new String'(Word);
      else
         Window.Window_Name := new String'("Default");
      end if;
   end Set_Name;

   procedure Set_Changes(Window : in Gui.Window.Window_Pointer) is
      Word : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
         Change_Window_Dialog_Window.Entry4);
   begin

      begin
         Window.Width := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Change_Window_Dialog_Window.Entry2);
      exception
         when others => Window.Width := -99;
      end;

      begin
         Window.Height := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Change_Window_Dialog_Window.Entry3);
      exception
         when others => Window.Height := -99;
      end;

      if Word'Last > Word'First then
         Window.Title := new String'(Word);
      else
         Window.Title := null;
      end if;
   end Set_Changes;

   -------------------------------------------------------------
   -- procedure Change_Done
   --
   -- Change is Done (hit ok)-- update display
   --
   -- Algorithm
   -- 1) Apply changes -- copy of below b/c we don't
   --    wan't to destroy window if changes not ok.
   --    Reset name between undisplay and display
   -- 2) Destroy dialog only if changes ok
   -------------------------------------------------------------
   procedure Change_Done is
      Window    : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      Set_Changes(Window);
      if Check_Changes(Window) then
         Gui.Window.Undisplay_Window(Window.all);
         Set_Name(Window);
         Gui.Window.Display_Window(Window.all);
         Mcc.Gui.Destroy(Mcc.Gui.Object(
            Change_Window_Dialog_Window.Change_Window_Dialog_Window));
      end if;
      State.Set_Changed(True);
   end Change_Done;

   procedure Change_Done(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Change_Done;
   end Change_Done;

   -------------------------------------------------------------
   -- procedure Apply_Changes
   --
   -- Update window with changes so far
   --
   -- Algorithm
   -- 1) Set changes into record
   -- 2) If OK, undisplay and redisplay, changing name between
   -- 3) Note the window has changed
   -------------------------------------------------------------
   procedure Apply_Changes is
      Window    : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      Set_Changes(Window);
      if Check_Changes(Window) then
         Gui.Window.Undisplay_Window(Window.all);
         Set_Name(Window);
         Gui.Window.Display_Window(Window.all);
      end if;
      State.Set_Changed(True);
   end Apply_Changes;

   procedure Apply_Changes(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Apply_Changes;
   end Apply_Changes;
   
   -------------------------------------------------------------
   -- procedure Cancel_Changes
   --
   -- Destroy dialog without doing anything
   -------------------------------------------------------------
   procedure Cancel_Changes is
      Window    : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      if Check_Changes(Window) then
         Mcc.Gui.Destroy(Mcc.Gui.Object(
            Change_Window_Dialog_Window.Change_Window_Dialog_Window));
      end if;
   end Cancel_Changes;

   procedure Cancel_Changes(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Cancel_Changes;
   end Cancel_Changes;

   -------------------------------------------------------------
   -- procedure Duplicate_Choice
   --
   -- duplicate a widget
   --
   -- 1) Can't do this if dialog is running, so just exit (ignore)
   --    Same if there is no window or selection
   -- 2) Create a new widget of the same class
   -- 3) Call Subwindow_Actions.Duplicate to complete
   -- 4) Note window has changed
   -------------------------------------------------------------
   procedure Duplicate_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Duplicate_Choice;
   end Duplicate_Choice;
   
   procedure Duplicate_Choice is
      Window    : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Selection : Gui.Widget.Widget_Access := State.Get_Selection;
      New_Guy   : Gui.Widget.Widget_Access;
   begin -- Duplicate_Choice
      -- don't do if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      if Window /= null and then Selection /= null then
         New_Guy := new Gui.Widget.GUI_Widget'Class'(Selection.all);
         Subwindow_Actions.Duplicate(New_Guy);
         State.Set_Changed(True);
      end if;
   end Duplicate_Choice;

   -------------------------------------------------------------
   -- procedure Delete_Choice
   --
   -- duplicate a widget
   --
   -- 1) Can't do this if dialog is running, so just exit (ignore)
   --    Same if there is no window or selection
   -- 2) Find selected widget in list (using name)
   -- 3) Undisplay widget
   -- 4) Remove highlighting (if widget was highlighted)
   -- 5) Delete from list
   -- 6) Note window has changed
   -------------------------------------------------------------
   procedure Delete_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Delete_Choice;
   end Delete_Choice;

   procedure Delete_Choice is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Position    : Gui.Widget.Widget_Pointer;
      OldPosition : Gui.Widget.Widget_Pointer;
      Selection   : Gui.Widget.Widget_Access;
   begin -- Delete_Choice
      -- don't do if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      if Window /= null then
         Position := Gui.Widget.Widget_List_Package.First(
            Window.Widget_List);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(
            Window.Widget_List,Position) loop
            Selection := Gui.Widget.Widget_List_Package.Retrieve(
               Window.Widget_List,Position);
            if Selection.Is_Selected then
               Gui.Widget.Undisplay_Widget(
                  Widget      => Selection.all);
               Gui.Widget.Unhighlight(
                  Widget      => Selection.all);
               OldPosition := Position;
               Gui.Widget.Widget_List_Package.GoAhead(
                  Window.Widget_List,Position);
               Gui.Widget.Widget_List_Package.Delete(
                  L => Window.Widget_List, P => OldPosition);
               State.Set_Changed(True);
            else
               Gui.Widget.Widget_List_Package.GoAhead(
                  Window.Widget_List,Position);
            end if;
         end loop;
      end if;

   end Delete_Choice;

end Edit_Menu;