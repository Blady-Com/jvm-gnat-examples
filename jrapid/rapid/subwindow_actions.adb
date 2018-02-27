---------------------------------------------------------------
--
--  RAPID - Rapid Ada Portable Interface Designer
--
--  SUBWINDOW_ACTIONS.ADB
--  Description : Those actions from events in a subwindow
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
-- 09/22/99 (mcc) : clicking off or dragging new deselects
-- 09/21/99 (mcc) : fixed Ctrl key problem for dialogs
--                  (RAPID thought Ctrl still pressed)
---------------------------------------------------------------
with Mcc.Common_Dialogs;
with Gui.Window;
with State;
with Toolbar;
with Gui;
with Gui.Widget;
use type Gui.String_Pointer,Gui.Widget.Widget_Access;
use type Gui.Window.Window_Pointer;
with Menu_Edit;
with Ada.Unchecked_Deallocation;
with Gui_Enum;
--with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
with Font_Actions;
with ada.text_io;

package body Subwindow_Actions is
   -- keep this around as global state for Done_Add_Widget callback
   -- and also for Done_Modify_Widget callback
   The_Widget  : Gui.Widget.Widget_Access;
   type Action is (Add,Modify);
   Move_Corner_X, Move_Corner_Y : Integer;
   Current_Action : Action;
   Fake_Name : Gui.String_Pointer; -- used to change name before verifying

   -- walk through widget list-- make sure nothing is highlighted
   procedure Unselect_Widgets is
      Window     : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Position : Gui.Widget.Widget_Pointer :=
         Gui.Widget.Widget_List_Package.First(Window.Widget_List);
      Current : Gui.Widget.Widget_Access;
   begin
      while not Gui.Widget.Widget_List_Package.IsPastEnd(
         Window.Widget_List,Position) loop
         Current := Gui.Widget.Widget_List_Package.Retrieve(
            Window.Widget_List,Position);
         if Current.Is_Selected then
            Gui.Widget.Unhighlight(Widget => Current.all);
            Current.Is_Selected := False;
         end if;
         Gui.Widget.Widget_List_Package.GoAhead(
            Window.Widget_List,Position);
      end loop;
   end Unselect_Widgets;

   procedure Add_Widget(Startx : in Integer; Starty : in Integer;
         Endx : in Integer; Endy : in Integer) is
      Widget_Type : Toolbar.Widget_Names := Toolbar.Get_Selected_Widget;
      Smallx : Integer := integer'Min(Startx,Endx);
      Smally : Integer := integer'Min(Starty,Endy);
      Bigx   : Integer := integer'Max(Startx,Endx);
      Bigy   : Integer := integer'Max(Starty,Endy);
      Width  : Integer := Bigx-Smallx;
      Height : Integer := Bigy-Smally;
   begin
      -- don't add if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      Unselect_Widgets;
      Current_Action := Add;
      if Width > 0 and then Height > 0 then
         The_Widget := Gui_Enum.Allocate_Widget(Widget_Type);
         Gui.Widget.Set_Location(The_Widget,
            Smallx, Smally, Width, Height);
         State.Set_Dialog_Running(True);
         Gui.Widget.Set_Properties(The_Widget.all);
      end if;
   end Add_Widget;

   -- really this is called from Edit_Menu, and probably doesn't
   -- belong here, but it was easier to reuse code from Add_Widget
   procedure Duplicate(Widget : in Gui.Widget.Widget_Access) is
   begin
      Current_Action := Add;
      State.Set_Dialog_Running(True);
      The_Widget     := Widget; -- was passed in newly allocated widget
      The_Widget.Name       := null;
      The_Widget.The_Widget := null; -- make sure we forget old widget
      Gui.Widget.Set_Properties(The_Widget.all);
   end Duplicate;

   function Find_Widget(Window : in Gui.Window.Window_Pointer;
         Name : in String) return Gui.Widget.Widget_Access is
      Real_Name_Start : Integer;
   begin
      -- note we only have stored after the last . in the widget
      Real_Name_Start := Name'Last+1;
      while Real_Name_Start > Name'First and then
            Name(Real_Name_Start-1) /= '.' loop
         Real_Name_Start := Real_Name_Start - 1;
      end loop;
      return
         Gui.Widget.Widget_List_Package.Retrieve(
         Window.Widget_List,
         Gui.Widget.Widget_Key_List_Package.Find(
            Key => Name(Real_Name_Start..Name'Last),
            Ptr => Window.Widget_List));
   exception when Gui.Widget.Widget_Key_List_Package.Not_Found =>
         return null;
   end Find_Widget;

   function Find_Widget(
         Window : in Gui.Window.Window_Pointer;
         Obj    : in Mcc.Gui.Widget.Widget'Class)
      return Gui.Widget.Widget_Access is
      -- This is kind of gory.  What I want to be able to do is
      -- walk through the list of widgets in the window and match
      -- up the one that was just clicked (or whatever)
      -- I take advantage of the fact that the RM requires the object
      -- to be passed by reference, so I convert the 'address to
      -- an access type, then do an unchecked_conversion to get the
      -- exact type I need.
      type fred is access constant Mcc.Gui.Widget.Widget'Class;
--      package Convert is new System.Address_To_Access_Conversions(
--         Object => Mcc.Gui.Widget.Widget'Class);
--      function UC is new Ada.Unchecked_Conversion(
--         Convert.Object_Pointer,Mcc.Gui.Widget.Widget_Pointer);
      function UC is new Ada.Unchecked_Conversion(
         fred,Mcc.Gui.Widget.Widget_Pointer);
   begin
      return
         Gui.Widget.Widget_List_Package.Retrieve(
         Window.Widget_List,
         Gui.Widget.Widget_Key2_List_Package.Find(
            Key => UC(Obj'Unchecked_Access),
            --            Key => UC(Convert.To_Pointer(Obj'Address)),
            Ptr => Window.Widget_List));
   exception when Gui.Widget.Widget_Key_List_Package.Not_Found =>
         return null;
   end Find_Widget;

   procedure Done_Add_Widget(Dismiss : in Boolean := True) is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Ok : Boolean;
      Temp_Name : Gui.String_Pointer;
      procedure Free is new Ada.Unchecked_Deallocation(String,
         Gui.String_Pointer);
   begin
      Gui.Widget.Apply_Properties(The_Widget.all);

      -- this is really grotesque, but we need to make sure the
      -- name isn't duplicated.  Swap in fake name and do a find.
      -- if found, then bad.  Then swap back in real name
      Temp_Name := The_Widget.Name;
      The_Widget.Name := Fake_Name;

      if Temp_Name /= null and then
            Find_Widget(Window,Temp_Name.all) /= null then
         Mcc.Common_Dialogs.Ok_Box(Temp_Name.all & " is already in use." &
            Ascii.Lf & "Pick another name");
         Free(Temp_Name);
         The_Widget.Name := null;
      else
         The_Widget.Name := Temp_Name;
      end if;

      Gui.Widget.Check_Properties(The_Widget.all,Ok);
      if not Ok then
         return;
      end if;


      if Dismiss then
         Gui.Widget.Close_Properties(The_Widget.all);
         State.Set_Dialog_Running(False);
      else
         Current_Action := Modify;
      end if;

      -- add to Window.Widget_List
      Gui.Widget.Widget_List_Package.Addtorear(L => Window.Widget_List,
         X => The_Widget);
      Gui.Widget.Display_Widget(Widget => The_Widget.all,
         Container => Window.Display_Area);
      State.Set_Changed(True);
   end Done_Add_Widget;

   procedure Modify_Widget(Obj : in out Mcc.Gui.Widget.Widget'Class) is
      Window     : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      -- don't modify if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      Unselect_Widgets;
      Current_Action := Modify;
      The_Widget := Find_Widget(Window,Obj);
      if The_Widget /= null then
         State.Set_Dialog_Running(True);
         Gui.Widget.Set_Properties(The_Widget.all);
      end if;
   end Modify_Widget;

   procedure Done_Modify_Widget(Dismiss : in Boolean := True) is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Selection  : Gui.Widget.Widget_Access := State.Get_Selection;
      Ok : Boolean;
      Temp_Name : Gui.String_Pointer;
      -- to have backup in case of change and then cancel
--      Widget_Backup : Gui.Widget.Gui_Widget'class := The_Widget.all;
      New_Widget_Backup : Gui.Widget.Widget_Access;
      procedure Free is new Ada.Unchecked_Deallocation(String,
         Gui.String_Pointer);
   begin
      New_Widget_Backup := new Gui.Widget.Gui_Widget'Class'(The_Widget.all);

      Gui.Widget.Apply_Properties(The_Widget.all);

      -- this is really grotesque, but we need to make sure the
      -- name isn't duplicated.  Swap in fake name and do a find.
      -- if found, then bad.  Then swap back in real name
      Temp_Name := The_Widget.Name;
      The_Widget.Name := Fake_Name;

      if Temp_Name /= null and then
            Find_Widget(Window,Temp_Name.all) /= null then
         Mcc.Common_Dialogs.Ok_Box(Temp_Name.all & " is already in use." &
            Ascii.Lf & "Pick another name");
         Free(Temp_Name);
         The_Widget.Name := null;
      else
         The_Widget.Name := Temp_Name;
      end if;

      Gui.Widget.Check_Properties(The_Widget.all,Ok);
      if not Ok then
         -- restores widget to old value in case cancel is selected
         --The_Widget.all := Widget_Backup;
         The_Widget.all := New_Widget_Backup.all;
         return;
      end if;

      if Dismiss then
         Gui.Widget.Close_Properties(The_Widget.all);
         State.Set_Dialog_Running(False);
      end if;
      Gui.Widget.Undisplay_Widget(Widget => The_Widget.all);
      Gui.Widget.Display_Widget(Widget => The_Widget.all,
         Container => Window.Display_Area);
      if Selection = The_Widget then
         Gui.Widget.Move_Highlight(Widget => The_Widget.all);
      end if;
      State.Set_Changed(True);
   end Done_Modify_Widget;

   procedure Cancel_Properties_Dialog is
      Ok : Boolean;
   begin
      -- can't quit if font dialog up
      if State.Font_Dialog_Running then
         return;
      end if;

      if Current_Action = Modify then
         Gui.Widget.Check_Properties(The_Widget.all,Ok);
         if Ok then
            Gui.Widget.Close_Properties(The_Widget.all);
         end if;
      else -- Add
         Gui.Widget.Close_Properties(The_Widget.all);
      end if;
      State.Control_Released;
   end Cancel_Properties_Dialog;

   procedure Done_Properties_Dialog is
   begin
      -- can't quit if font dialog up
      if State.Font_Dialog_Running then
         return;
      end if;

      if Current_Action = Add then
         Done_Add_Widget;
      elsif Current_Action = Modify then
         Done_Modify_Widget;
      end if;
      State.Control_Released;
   end Done_Properties_Dialog;

   procedure Apply_Properties_Dialog is
   begin
      -- can't quit if font dialog up
      if State.Font_Dialog_Running then
         return;
      end if;

      if Current_Action = Add then
         Done_Add_Widget(Dismiss => False);
      elsif Current_Action = Modify then
         Done_Modify_Widget(Dismiss => False);
      end if;
   end Apply_Properties_Dialog;

   procedure Select_Widget(Obj : in out Mcc.Gui.Widget.Widget'Class) is
      Window     : Gui.Window.Window_Pointer := State.Get_Current_Window;
      --Selection  : Gui.Widget.Widget_Access := State.Get_Selection;
      Position : Gui.Widget.Widget_Pointer :=
         Gui.Widget.Widget_List_Package.First(Window.Widget_List);
      --Current : Gui.Widget.Widget_Access;
   begin
      -- don't select if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      if not State.Is_Control_Pressed then
         Unselect_Widgets;
      end if;

      The_Widget := Find_Widget(Window,Obj);
      if The_Widget /= null then
         Move_Corner_X := The_Widget.X;
         Move_Corner_Y := The_Widget.Y;
         -- don't allow us to try to highlight this widget a second time
         if not The_Widget.Is_Selected then
            The_Widget.Is_Selected := True;
            Gui.Widget.Highlight(Widget => The_Widget.all);
         end if;
      end if;

   end Select_Widget;

   procedure Edit_Menu(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Edit_Menu;
   end Edit_Menu;

   procedure Edit_Menu is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      -- don't edit if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      if Window /= null then
         Menu_Edit.Edit(Window.Menu,Window.Display_Area);
      end if;
   end Edit_Menu;


   procedure Move_Widget(
         Obj    : in out Mcc.Gui.Widget.Widget'Class;
         startx : in     Integer;
         starty : in     Integer;
         endx   : in     Integer;
         endy   : in     Integer) is
      Window      : Gui.Window.Window_Pointer := State.Get_Current_Window;
      Current     : Gui.Widget.Widget_Access;
      Position    : Gui.Widget.Widget_Pointer;
   begin
      -- don't move if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      --The_Widget := Find_Widget(Window,Obj);
      Position := Gui.Widget.Widget_List_Package.First(
         Window.Widget_List);
      while not Gui.Widget.Widget_List_Package.IsPastEnd(
         Window.Widget_List,Position) loop
         Current := Gui.Widget.Widget_List_Package.Retrieve(
            Window.Widget_List,Position);
         if Current.Is_Selected then
            -- this is really peculiar code, but
            -- start and end are relative to the upper left corner
            -- of the widget, which moves as we go
            Current.X      := Current.X + Endx-Startx;
            Current.Y      := Current.Y + Endy-Starty;
            Mcc.Gui.Widget.Move(
               Obj => Current.The_Widget.all,
               X   => Current.X,
               Y   => Current.Y);
            Gui.Widget.Move_Highlight(Widget => Current.all);
         end if;
         Gui.Widget.Widget_List_Package.GoAhead(
            Window.Widget_List,Position);
      end loop;
      State.Set_Changed(True);
   end Move_Widget;


   procedure Resize_Widget(Handle : in String;
         Startx : in Integer; Starty : in Integer;
         Endx   : in Integer; Endy   : in Integer) is
      The_Widget  : Gui.Widget.Widget_Access := State.Get_Selection;
   begin
      -- don't move if dialog already running
      if State.Dialog_Running then
         return;
      end if;

      if The_Widget /= null then
         -- this is really peculiar code, but
         -- start and end are relative to the upper left corner
         -- of the handle, which moves as we go
         if Handle = "nw" then
            The_Widget.X      := The_Widget.X + Endx-Startx;
            The_Widget.Y      := The_Widget.Y + Endy-Starty;
            The_Widget.Width  := The_Widget.Width  + Startx-Endx;
            The_Widget.Height := The_Widget.Height + Starty-Endy;
         elsif Handle = "ne" then
            The_Widget.Y      := The_Widget.Y + Endy-Starty;
            The_Widget.Width  := The_Widget.Width  + Endx-Startx;
            The_Widget.Height := The_Widget.Height + Starty-Endy;
         elsif Handle = "sw" then
            The_Widget.X      := The_Widget.X + Endx-Startx;
            The_Widget.Width  := The_Widget.Width  + Startx-Endx;
            The_Widget.Height := The_Widget.Height + Endy-Starty;
         elsif Handle = "se" then
            The_Widget.Width  := The_Widget.Width  + Endx-Startx;
            The_Widget.Height := The_Widget.Height + Endy-Starty;
         elsif Handle = "n" then
            The_Widget.Y      := The_Widget.Y + Endy-Starty;
            The_Widget.Height := The_Widget.Height + Starty-Endy;
         elsif Handle = "w" then
            The_Widget.X      := The_Widget.X + Endx-Startx;
            The_Widget.Width  := The_Widget.Width  + Startx-Endx;
         elsif Handle = "s" then
            The_Widget.Height := The_Widget.Height + Endy-Starty;
         elsif Handle = "e" then
            The_Widget.Width  := The_Widget.Width  + Endx-Startx;
         end if;
         Mcc.Gui.Widget.Move(
            Obj => The_Widget.The_Widget.all,
            X   => The_Widget.X,
            Y   => The_Widget.Y);
         Mcc.Gui.Widget.Resize(
            Obj    => The_Widget.The_Widget.all,
            Width  => The_Widget.Width,
            Height => The_Widget.Height);
         Gui.Widget.Move_Highlight(Widget => The_Widget.all);
         State.Set_Changed(True);
      end if;
   exception when others => null;
   end Resize_Widget;

   procedure Done_Properties_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Done_Properties_Dialog;
   end Done_Properties_Dialog;

   procedure Apply_Properties_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Apply_Properties_Dialog;
   end Apply_Properties_Dialog;

   procedure Cancel_Properties_Dialog(
      Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Cancel_Properties_Dialog;
   end Cancel_Properties_Dialog;

   procedure Cancel_Properties_Dialog(
      Obj : in out Mcc.Gui.Container.Window.Window'Class) is
   begin
      Cancel_Properties_Dialog;
   end Cancel_Properties_Dialog;

   procedure Change_Font(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      State.Set_Font_Dialog_Running(True);
      Font_Actions.Change_Font(The_Widget);
   end Change_Font;

   procedure Change_Font_Done is
   begin
      State.Set_Font_Dialog_Running(False);
   end Change_Font_Done;

begin
   Fake_Name := new String'("dontuseme" & Character'Last);
end Subwindow_Actions;
