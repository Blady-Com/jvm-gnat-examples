---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  FILE_MENU.ADB
--  Description : Implements choices from the File menu
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
with State;
with Gui.Window;
use type Gui.Window.Window_Pointer;
with Mcc.Common_Dialogs;
with Limits;
with new_dialog_Window;
with ada.exceptions;
with Mcc.Gui.Container.Window;
with Mcc.Gui.Widget.Text_Entry;

package body File_Menu is
   New_Running : Boolean := False;
   -- Is a new dialog running already?


   -- used in File Open/Save dialogs to select
   -- which files are displayed
   File_Types : constant String :=
      "{ ""Gui Files"" { .gui } } " &
      "{ ""All Files"" * }";

   -- Holds file name and Directory name returned from
   -- File dialogs
   Filename       : String(1..Limits.Max_Name_Length);
   Filename_Last  : Natural;
   Directory      : String(1..Limits.Max_Name_Length);
   Directory_Last : Natural;

   -----------------------------------------------------------
   -- procedure New_Choice
   -- 
   -- called when user pushes OK in New Dialog (.new_dialog)
   --
   -- 1) Read entry1 (name)
   -- 2) Change first letter to lower case (for Tcl)
   -- 3) Replace other spaces by underscores (for Tcl)
   -- 4) Replace main by "." (for Tcl)
   -- 5) Read width and height
   -- 6) Allocate window, setting current Filename to "Untitled"
   --    (see Save for use of this) and Directory to current (.)
   -- 7) Create and display window
   -- 8) Note that window is unchanged
   -----------------------------------------------------------
   procedure New_Done is
      Window_Name  :          String  := Mcc.Gui.Widget.Text_Entry.
         Get_Text(New_Dialog_Window.Entry1);
      Window_Last  :          Natural := Window_Name'Last;
      Title        : constant String  := Mcc.Gui.Widget.Text_Entry.
         Get_Text(New_Dialog_Window.Entry4);
      Width,Height : Integer;
   begin
      -- eliminate trailing spaces
      while Window_Last > Window_Name'First and then
         Window_Name(Window_Last) = ' ' loop
         Window_Last := Window_Last -1;
      end loop;

      -- get rid of other spaces
      for i in Window_Name'First..Window_Last loop
         if Window_Name(i) = ' ' then
            Window_name(i) := '_';
         end if;
      end loop;

      Width := Mcc.Gui.Widget.Text_Entry.Get_Text(
         New_Dialog_Window.Entry2);

      Height := Mcc.Gui.Widget.Text_Entry.Get_Text(
         New_Dialog_Window.Entry3);


      Mcc.Gui.Destroy(Mcc.Gui.Object(New_Dialog_Window.New_Dialog_Window));
      
      State.Set_Current_Window(State.Allocate_Window);
      State.Set_Filename("Untitled");
      State.Set_Directory(".");

      Gui.Window.Create_Window(Window => State.Get_Current_Window.all,
         Width       => Width,
         Height      => Height,
         Filename    => "Untitled",
         Title       => Title,
         Window_Name => Window_Name(Window_Name'First..Window_Last));

      Gui.Window.Display_Window(
         Window => State.Get_Current_Window.all);
      State.Set_Changed(False);
      State.Control_Released;
      New_Running := False;
   end New_Done;
   procedure New_Done(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      New_Done;
   end New_Done;

   -----------------------------------------------------------
   -- procedure Cancel_New
   --
   -- Set New_Running to false
   -----------------------------------------------------------
   procedure Cancel_New is
   begin
      Mcc.Gui.Destroy(Mcc.Gui.Object(New_Dialog_Window.New_Dialog_Window));
      New_Running := False;
   end Cancel_New;

   procedure Cancel_New(Obj : in out Mcc.Gui.Container.Window.Window'Class) is
   begin
      Cancel_New;
   end Cancel_New;

   -----------------------------------------------------------
   -- procedure New_Choice
   --
   -- 0) if New already running, then return (set New_Running)
   -- 1) Close old file (remove if MDI implemented)
   -- 2) Generate new dialog
   -- 2b) Set window close to Cancel_New (see tcl_bindings.txt)
   -- 3) Set default size to 300x300 in dialog
   -- 4) Set focus on name (entry1)
   -- 5) Exit as rest is done in New_Done
   -----------------------------------------------------------
   procedure New_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      New_Choice;
   end New_Choice;

   procedure New_Choice is
   begin -- New_Choice
      if New_Running then
         return;
      end if;
      New_Running := True;

      Close_Choice;  -- remove for MDI

      New_Dialog_Window.Generate_Window;
      
      Mcc.Gui.Container.Window.Set_Close_Handler(
         Obj     => New_Dialog_Window.New_Dialog_Window'access,
         Handler => Cancel_New'access);

      -- set initial width and height to 300
      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj     => New_Dialog_Window.Entry2,
         Text    => "300");
         
      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj     => New_Dialog_Window.Entry3,
         Text    => "300");

      Mcc.Gui.Widget.Text_Entry.Highlight(
         Obj   => New_Dialog_Window.Entry1);
   end New_Choice;

   -----------------------------------------------------------
   -- procedure Open_Choice
   -- 
   -- 1) Display dialog
   -- 2) If not cancelled, run close choice (remove for MDI)
   --    this gives opportunity to save previous
   -- 3) Allocate window, setting filename and directory
   --    to result from dialog
   -- 4) Call Read_Window from Gui_Window to read file
   -- 5) Display the Window
   -- 6) Note window is unchanged
   -----------------------------------------------------------
   procedure Open_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Open_Choice;
   end Open_Choice;
   
   procedure Open_Choice is

   begin -- Open_Choice
      Mcc.Common_Dialogs.Open_Dialog(
         File_Types => File_Types,
         Filename   => Filename,
         File_Last  => Filename_Last,
         Directory  => Directory,
         Dir_Last   => Directory_Last);
      State.Control_Released;
      if Filename_Last > 0 then
         -- remove for MDI
         Close_Choice;

         State.Set_Current_Window(State.Allocate_Window);
         State.Set_Filename(Filename(1..Filename_Last));
         State.Set_Directory(Directory(1..Directory_Last));
         Gui.Window.Read_Window(Window => State.Get_Current_Window.all,
            Filename => State.Get_Filename);
         Gui.Window.Display_Window(
            Window => State.Get_Current_Window.all);
         State.Set_Changed(False);
      end if;
   exception
      when e:others =>
         State.Set_Current_Window(null);
         Mcc.Common_Dialogs.OK_Box("Unable to open: " &
            Filename(1..Filename_Last) & ASCII.LF &
            "Not a valid GUI file?" & ASCII.LF &
            Ada.Exceptions.Exception_Information(E));
   end Open_Choice;


   -----------------------------------------------------------
   -- procedure Close_Choice
   -- 
   -- 1) If a window is not open, ignore
   -- 2) If window has changed, ask if they want to save
   --    2b) If so, run Save_Choice
   -- 3) Undisplay window, set current to null (change for MDI)
   -----------------------------------------------------------
   procedure Close_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Close_Choice;
   end Close_Choice;

   procedure Close_Choice is
      Window : Gui.Window.Window_Pointer;
   begin -- Close_Choice
      Window := State.Get_Current_Window;

      if Window /= null then
         if State.Get_Changed then
            if Mcc.Common_Dialogs.Yesno_Dialog(
               Title   => "Save work?",
               Message => "Do you want to save changes to " &
                  State.Get_Filename & "?") then
               Save_Choice;
            end if;
         end if;
         Gui.Window.Undisplay_Window(
            Window => State.Get_Current_Window.all);
      end if;
      State.Set_Current_Window(null);
      State.Control_Released;
   end Close_Choice;


   -----------------------------------------------------------
   -- procedure Save_Choice
   -- 
   -- 1) If no current window, then ignore
   -- 2) If window name is "Untitled" (see New_Choice)
   --    then go to SaveAs_Choice 
   -- 3) Otherwise, ask Window to write itself
   -- 4) Mark window as unchanged
   -----------------------------------------------------------
   procedure Save_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      Save_Choice;
   end Save_Choice;

   procedure Save_Choice is

   begin -- Save_Choice
      if State.Get_Current_Window /= null then
         if State.Get_Filename = "Untitled" then
            SaveAs_Choice;
         else
            Gui.Window.Write_Window(Window => State.Get_Current_Window.all);
            State.Set_Changed(False);
         end if;
      end if;
   end Save_Choice;


   -----------------------------------------------------------
   -- procedure SaveAs_Choice
   -- 
   -- 1) if no Window, ignore
   -- 2) Put up Save dialog
   -- 3) if we get a filename (not pressed cancel) then
   --    3b) Change window name
   --    3c) Set new filename and directory
   --    3d) call Save to perform write
   -----------------------------------------------------------
   procedure SaveAs_Choice(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      SaveAs_Choice;
   end SaveAs_Choice;

   procedure SaveAs_Choice is

   begin -- SaveAs_Choice
      if State.Get_Current_Window /= null then
         Mcc.Common_Dialogs.Save_Dialog(
            File_Types => File_Types,
            Filename   => Filename,
            File_Last  => Filename_Last,
            Directory  => Directory,
            Dir_Last   => Directory_Last);
         State.Control_Released;
         if Filename_Last > Filename'First then
            State.Set_Filename(Filename(1..Filename_Last));
            State.Set_Directory(Directory(1..Directory_Last));
            Gui.Window.Change_Filename(
               Window   => State.Get_Current_Window.all,
               Filename => State.Get_Filename);
            Save_Choice;
         end if;
      end if;
   end SaveAs_Choice;


   -----------------------------------------------------------
   -- procedure Exit_Choice
   -- 
   -- 1) Close file (to allow possible save)
   -- 2) Quit (do not ask)
   -----------------------------------------------------------
   procedure Exit_Choice(Obj : in out Mcc.Gui.Container.Window.Window'Class) is
   begin
      Exit_Choice;
   end Exit_Choice;

   procedure Exit_Choice is

   begin -- Exit_Choice
      Close_Choice;
      Mcc.Common_Dialogs.Quit_Dialog(Verify => False);
      State.Control_Released;
   end Exit_Choice;

end File_Menu;
