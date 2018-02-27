---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  GUI-WINDOW.ADB 
--  Description : Root of GUI Window Hierarchy
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
with Gui_Enum;
use type Gui_Enum.Keyword;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Menu_IO,Widget_IO,Menu_Generate;
with Gui.Menu;
with File_Helpers;
with Ada.Unchecked_Deallocation;
use type Gui.Menu.Menu_Pointer;
with Gui.Widget;
use type Gui.Widget.Widget_Pointer;
with Ada.Tags;
use type Ada.Tags.Tag;
with Ada.Characters.Handling;
with Mcc.Gui.Widget.Label;
with Mcc.Gui.Container.Frame;
with Mcc.Gui.Container.Window;
with Mcc.Gui.Colors;
with Main_Window;
with Mcc.Common_Dialogs;
with Ada.Exceptions;
with State;
with Subwindow_Actions;

package body Gui.Window is
   -- size/location of subframes
   Menu_Start_Y   : constant := 70;
   Canvas_Start_Y : constant := 95;
   Menu_Height    : Integer  := Canvas_Start_Y - Menu_Start_Y;
   Resizer_Size   : constant := 6;
   -- extra past size of window
   Extra_X     : constant := 10;
   Extra_Y     : constant := 10;
   --------------------------------------------------------------
   -- Initialize a GUI_Window
   --------------------------------------------------------------
   procedure Create_Window(Window : in out Gui_Window; Filename : in String;
         Width       : in Integer;
         Height      : in Integer;
         Title       : in String;
         Window_Name : in String) is
   begin
      Window.Filename := new String'(Filename);

      if Window_Name'Length = 0 then
         Window.Window_Name := new String'("main");
      else
         Window.Window_Name := new String'(Window_Name);
      end if;

      if Title'Length = 0 then
         Window.Title := null;
      else
         Window.Title := new String'(Title);
      end if;

      Window.Width := Width;
      Window.Height := Height;
      Gui.Widget.Widget_List_Package.Initialize(Window.Widget_List);
      Gui.Menu.Menu_List_Package.Initialize(Window.Menu);
   end Create_Window;

   --------------------------------------------------------------
   procedure Resize_Window(
         Window : in out Gui_Window;
         Width  : in Integer;
         Height : in Integer) is
   begin
      Window.Width  := Width;
      Window.Height := Height;
      Mcc.Gui.Resize(
         Obj    => Mcc.Gui.Sized_Object(Window.Display_Area),
         Width  => Window.Width,
         Height => Window.Height);
      Mcc.Gui.Resize(
         Obj    => Mcc.Gui.Sized_Object(Window.Menu_Area),
         Width  => Window.Width,
         Height => Menu_Height);
      Mcc.Gui.Move(
         Obj => Mcc.Gui.Sized_Object(Window.Resize_Frame),
         X   => Window.Width,
         Y   => Window.Height + Canvas_Start_Y);
      Mcc.Gui.Container.Window.Resize(
         Obj    => Main_Window.Main_Window,
         Width  => integer'max(Window.Width + Extra_X,
            Limits.Min_Rapid_Window_Width),
         Height => integer'max(Window.Height + Canvas_Start_Y + Extra_Y,
            Limits.Min_Rapid_Window_Height));
   end Resize_Window;


   --------------------------------------------------------------
   -- reads information from file into GUI_Window
   --------------------------------------------------------------
   procedure Read_Window(Window : in out Gui_Window; Filename : in String) is
      Keyword    : Gui_Enum.Keyword;
      Word       : Word_Type;
      Last       : Natural;
      File       : Ada.Text_IO.File_Type;
      Menu_Count : Integer;
   begin -- Read_Window
      Window.Filename := new String'(Filename);

      Ada.Text_IO.Open(File => File, Name => Window.Filename.all,
         Mode => Ada.Text_IO.In_File);

      Gui_Enum.IO.Get(File,Keyword);
      if Keyword /= Gui_Enum.Window then
         raise Bad_File;
      end if;

      File_Helpers.Get_String(File,Word,Last);
      Window.Window_Name := new String'(Word(Word'First..Last));

      -- get window title if present
      File_Helpers.Get_String(File,Word,Last);
      if Last >= Word'First then
         Window.Title := new String'(Word(Word'First..Last));
      else
         Window.Title := null;
      end if;

      Ada.Integer_Text_IO.Get(File,Window.Width);
      Ada.Integer_Text_IO.Get(File,Window.Height);

      Gui_Enum.IO.Get(File,Keyword);
      if Keyword = Gui_Enum.Menubar then
         Menu_Count := 1;
         Menu_IO.Read_Menubar(File,Window.Menu,Menu_Count);
         Gui_Enum.IO.Get(File,Keyword);
      else
         Gui.Menu.Menu_List_Package.Initialize(Window.Menu);
      end if;

      Gui.Widget.Widget_List_Package.Initialize(Window.Widget_List);
      if Keyword = Gui_Enum.Widgets then
         Widget_IO.Read_Widgets(File,
            Window.Window_Name.all, Window.Widget_List);
         Gui_Enum.IO.Get(File,Keyword);
      end if;

      -- read keyword in one of previous if statements
      if Keyword /= Gui_Enum.EndOf then
         raise Bad_File;
      end if;

      Gui_Enum.IO.Get(File,Keyword);
      if Keyword /= Gui_Enum.Window then
         raise Bad_File;
      end if;

      Ada.Text_IO.Close(File => File);
   exception when Menu_IO.Bad_File =>
         raise Bad_File;
   end Read_Window;

   --------------------------------------------------------------
   -- Writes information to file from GUI_Window
   --------------------------------------------------------------
   procedure Write_Window(Window : in Gui_Window) is
      File : Ada.Text_IO.File_Type;
   begin -- Write_Window
      Ada.Text_IO.Create(File => File,
         Name => Ada.Characters.Handling.To_Lower(Window.Filename.all),
         Mode => Ada.Text_IO.Out_File);

      GUI_Enum.IO.Put(File,Gui_Enum.Window);
      Ada.Text_IO.Put(File," """);
      Ada.Text_IO.Put(File,Window.Window_Name.all);
      Ada.Text_IO.Put(File,"""");

      -- for backward compatibility, we keep title on
      -- same line as name.  This way, Get_String will
      -- return nothing on older .GUI files in the
      -- read.
      if Window.Title /= null then
         Ada.Text_IO.Put(File," """);
         File_Helpers.Put_String(File,Window.Title.all);
         Ada.Text_IO.Put(File,"""");
      end if;

      Ada.Text_IO.New_Line(File);

      Ada.Integer_Text_IO.Put(File,Window.Width);
      Ada.Integer_Text_IO.Put(File,Window.Height);
      Ada.Text_IO.New_Line(File => File, Spacing => 2);

      if not Gui.Menu.Menu_List_Package.IsEmpty(Window.Menu) then
         Menu_IO.Write_Menubar(File,Window.Menu);
         Ada.Text_IO.New_Line(File => File);
      end if;

      if not Gui.Widget.Widget_List_Package.IsEmpty(Window.Widget_List) then
         Widget_IO.Write_Widgets(File => File, Widgets => Window.Widget_List);
         Ada.Text_IO.New_Line(File => File);
      end if;

      GUI_Enum.IO.Put(File,Gui_Enum.Endof);
      Ada.Text_IO.Put(File,' ');
      GUI_Enum.IO.Put(File,Gui_Enum.Window);
      Ada.Text_IO.New_Line(File);

      Ada.Text_IO.Close(File => File);
   end Write_Window;

   Start_X, Start_Y : Integer;
   procedure Mouse_Resizer(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
      use Mcc.Gui;
      Window      : Window_Pointer := State.Get_Current_Window;
   begin
      if Event.Button = Left then
         if Event.Action = Press then
            Start_X := Event.X;
            Start_Y := Event.Y;
         elsif Event.Action = Move then
            Resize_Window(Window.all,Start_X,Start_Y,Event.X,Event.Y);  
            State.Set_Changed(True);
         end if;
      end if;
   end Mouse_Resizer;
   
   Draw_Frame : Mcc.Gui.Container.Frame.Frame;
   procedure Display_Area_Mouse(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
      use Mcc.Gui;
   begin
      if Event.Button = Left then
         if Event.Action = Press then
            Start_X := Event.X;
            Start_Y := Event.Y;
            Mcc.Gui.Container.Frame.Create(
               Obj    => Draw_Frame,
               X      => Start_X,
               Y      => Start_Y,
               Width  => 0,
               Height => 0,
               Parent => Mcc.Gui.Container.Container'Class(Obj));
         elsif Event.Action = Move then
            Mcc.Gui.Container.Frame.Move(
               Obj    => Draw_Frame,
               X      => integer'min(Event.X,Start_X),
               Y      => integer'min(Event.Y,Start_Y));
            Mcc.Gui.Container.Frame.Resize(
               Obj    => Draw_Frame,
               Width  => abs(Event.X-Start_X),
               Height => abs(Event.Y-Start_Y));  
         elsif Event.Action = Release then
            Mcc.Gui.Container.Frame.Destroy(Draw_Frame);
            Subwindow_Actions.Add_Widget(
               Start_X,
               Start_Y,
               Event.X,
               Event.Y);
         end if;
      end if;
   exception
      when others => null;
   end Display_Area_Mouse;
   
   --------------------------------------------------------------
   -- display the GUI Window
   --------------------------------------------------------------
   procedure Display_Window(Window : in out Gui_Window) is

   begin
      if Window.Title /= null then
         Mcc.Gui.Widget.Label.Set_Text(
            Obj  => Main_Window.Name,
            Text => Window.Title.all);
      else
         Mcc.Gui.Widget.Label.Set_Text(
            Obj  => Main_Window.Name,
            Text => Window.Window_Name.all);
      end if;
      Mcc.Gui.Container.Frame.Create(
         Obj    => Window.Menu_Area,
         Parent => Main_Window.Main_Window,
         X      => 0,
         Y      => Menu_Start_Y,
         Width  => Window.Width,
         Height => Menu_Height);
      Mcc.Gui.Container.Frame.Create(
         Obj    => Window.Display_Area,
         Parent => Main_Window.Main_Window,
         X      => 0,
         Y      => Canvas_Start_Y,
         Width  => Window.Width,
         Height => Window.Height);

      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Window.Display_Area'unchecked_access,
         Listener => Display_Area_Mouse'access);

      begin
         Menu_Generate.Display_Menu_Code(
            Menubar => Window.Menu,
            Window  => Window);
      exception
         when e:others => Mcc.Common_Dialogs.OK_Box("Invalid menus: " &
                  ada.exceptions.exception_information(e));
      end;

      if not Gui.Widget.Widget_List_Package.IsEmpty(Window.Widget_List) then
         begin
            Widget_IO.Display_Widgets(
               Window    => Window,
               Widgets   => Window.Widget_List);
         exception
            when e:others =>
               Mcc.Common_Dialogs.OK_Box("Invalid widgets: " &
                  ada.exceptions.exception_information(e));
         end;
      end if;

      Mcc.Gui.Container.Frame.Create(
         Obj    => Window.Resize_Frame,
         Parent => Main_Window.Main_Window,
         X      => Window.Width,
         Y      => Window.Height+Canvas_Start_Y,
         Width  => Resizer_Size,
         Height => Resizer_Size);
         
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Window.Resize_Frame),
         Cursor => Mcc.Gui.Resize_SE);
         
      Mcc.Gui.Set_Background_Color(
         Obj       => Mcc.Gui.Sized_Object(Window.Resize_Frame),
         New_Color => Mcc.Gui.Colors.Named_Color(Mcc.Gui.Colors.Black));

      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Window.Resize_Frame'unchecked_access,
         Listener => Mouse_Resizer'access);
         
      Mcc.Gui.Container.Window.Resize(
         Obj    => Main_Window.Main_Window,
         Width  => integer'max(Window.Width + Extra_X,
            Limits.Min_Rapid_Window_Width),
         Height => integer'max(Window.Height + Canvas_Start_Y + Extra_Y,
            Limits.Min_Rapid_Window_Height));

      Mcc.Gui.Container.Window.Set_Title(
         Main_Window.Main_Window,
         Window.Filename.all);
   exception
      when e:others =>
         Mcc.Common_Dialogs.OK_Box("Invalid window:" &
            ada.exceptions.exception_information(e));
   end Display_Window;

   --------------------------------------------------------------
   --------------------------------------------------------------
   procedure Resize_Window(
         Window : in out Gui_Window;
         startx : in Integer;
         starty : in Integer;
         endx   : in Integer;
         endy   : in Integer) is
   begin
      Resize_Window(
         Window => Window,
         Width  => Window.Width  + (endx-startx),
         Height => Window.Height + (endy-starty));
   end Resize_Window;


   --------------------------------------------------------------
   -- destroy the GUI Window and its resize button
   -- change title of Window
   --------------------------------------------------------------
   procedure Undisplay_Window(Window : in Gui_Window) is
   begin
      Mcc.Gui.Destroy(Mcc.Gui.Object(Window.Display_Area));
      Mcc.Gui.Destroy(Mcc.Gui.Object(Window.Menu_Area));
      Mcc.Gui.Destroy(Mcc.Gui.Object(Window.Resize_Frame));
      Mcc.Gui.Widget.Label.Set_Text(
         Obj  => Main_Window.Name,
         Text => "No Window Open");
      Mcc.Gui.Container.Window.Set_Title(
         Main_Window.Main_Window,"RAPID");
   end Undisplay_Window;

   --------------------------------------------------------------
   -- Generate Ada code for this window.
   --------------------------------------------------------------
   procedure Generate_Window(Window : in Gui_Window) is
      -- Change window name as follows:
      -- 1) change "." to "main"
      -- 2) replace '.' and ' ' with '_'
      -- 3) remove all trailing '_'
      function Convert_Window_Name(Window_Name : in String) return String is
         Result : String := Window_Name;
         Start  : Integer;
      begin
         if Window_Name = "." then
            return "main";
         else
            for I in Result'range loop
               if Result(I) = '.' or else Result(I) = ' ' then
                  Result(I) := '_';
               end if;
            end loop;
         end if;

         Start := Result'First;
         while Result(Start) = '_' loop
            Start := Start + 1;
         end loop;

         return Result(Start..Result'Last);
      end Convert_Window_Name;

      -- wbw 6/6/99
      -- Generate spec for the window that looks like:
      -- -- Auto generated...
      -- -- By: Martin C. Carlisle and C2C W. Blair Watkinson
      -- package filename is
      --    procedure Generate_Window;
      -- end filename;
      -- with statements;
      -- package filename is
      --    newButton : Mcc.Gui.Widget.Button.Picture_Button.Picture_Button;
      --    openButton :
      --    saveButton :
      --    cutButton:
      --    compileButton:
      --    ...
      --
      --    Window : Mcc.Gui.Container.Window.Subwindow;
      --
      --    procedure Generate_Window;
      --
      -- end filename;
      Already_Context_Widgets : Gui.Widget.Widget_List;
      function Already_Context(Widget : in Gui.Widget.Widget_Access) return Boolean is
         Current_Widget_Position : Gui.Widget.Widget_List_Package.Position;
         Found                   : Boolean := False;
      begin
         Current_Widget_Position := Gui.Widget.Widget_List_Package.First(L => Already_Context_Widgets);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(Already_Context_Widgets,
            P => Current_Widget_Position) loop
            if Gui.Widget.Widget_List_Package.Retrieve(L => Already_Context_Widgets,
               P => Current_Widget_Position).all'Tag = Widget'Tag then
               Found := True;
               exit;
            end if;
            Gui.Widget.Widget_List_Package.GoAhead(L => Already_Context_Widgets,
               P => Current_Widget_Position);
         end loop;
         return Found;
      end Already_Context;

      procedure Generate_Spec(Filename : in String;
            Widget_List : in Gui.Widget.Widget_List) is
         Spec_File               : Ada.Text_Io.File_Type;
         Current_Widget_Position : Gui.Widget.Widget_Pointer;
         Current_Widget          : Gui.Widget.Widget_Access;
         NoMenu                  : Boolean;
      begin
         NoMenu := Gui.Menu.Menu_List_Package.IsEmpty(Window.Menu);
         Ada.Text_IO.Create(File => Spec_File, Mode => Ada.Text_IO.Out_File,
            Name => Filename & ".ads");
         Ada.Text_IO.Put_Line(Spec_File,"-- Auto generated by RAPID");
         Ada.Text_IO.Put_Line(Spec_File,"-- By: Martin C. Carlisle and W. Blair Watkinson II");

         if not NoMenu then
            Ada.Text_IO.Put_Line(File => Spec_File,
               Item => "with Mcc.Gui.Menu;");
         end if;

         Current_Widget_Position := Gui.Widget.Widget_List_Package.First(L => Widget_List);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(L => Widget_List,
            P => Current_Widget_Position) loop
            Current_Widget := Gui.Widget.Widget_List_Package.Retrieve(L => Widget_List,
               P => Current_Widget_Position);
            if not Already_Context(Current_Widget) then
               Gui.Widget.Widget_List_Package.AddToFront(L => Already_Context_Widgets,
                  X => Current_Widget);
            end if;
            Gui.Widget.Widget_List_Package.GoAhead(L => Widget_List,
               P => Current_Widget_Position);
         end loop;

         Current_Widget_Position := Gui.Widget.Widget_List_Package.First(L => Already_Context_Widgets);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(L => Already_Context_Widgets,
            P => Current_Widget_Position) loop
            Gui.Widget.Generate_Widget_Context_Clause
               (Widget => Gui.Widget.Widget_List_Package.Retrieve(L => Already_Context_Widgets,
                   P => Current_Widget_Position).all,
                File => Spec_File);
            Gui.Widget.Widget_List_Package.GoAhead(L => Already_Context_Widgets,
               P => Current_Widget_Position);
         end loop;

         Ada.Text_IO.Put_Line(File => Spec_File, Item => "with Mcc.Gui.Container.Window;");

         Ada.Text_IO.New_Line(File => Spec_File);
         Ada.Text_IO.Put_Line(Spec_File,
            "package " & Filename & " is");

         -- declarations (menus)
         if not NoMenu then
            Ada.Text_IO.Put_Line(File => Spec_File,
               Item => "   " & FileName(FileName'First..FileName'Last-7) & "_Menu" &
                       " : aliased Mcc.Gui.Menu.Window_Menu;");
            Gui.Menu.Generate_Menu_Declaration(Menu => Window.Menu,
               File => Spec_File);
         end if;

         -- declarations (widgets)
         Current_Widget_Position := Gui.Widget.Widget_List_Package.First(L => Widget_List);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(L => Widget_List,
            P => Current_Widget_Position) loop
            Gui.Widget.Generate_Widget_Declaration
               (Widget => Gui.Widget.Widget_List_Package.Retrieve(L => Widget_List,
                   P => Current_Widget_Position).all,
                File => Spec_File);
            Gui.Widget.Widget_List_Package.GoAhead(L => Widget_List,
               P => Current_Widget_Position);
         end loop;
         Widget_Io.Generate_Radio_Groups(
            File    => Spec_File,
            Widgets => Widget_List);

         Ada.Text_IO.New_Line(File => Spec_File);
         if Ada.Characters.Handling.To_Lower(
               FileName(FileName'First..FileName'Last-7)) = "main" then
            Ada.Text_IO.Put_Line(Spec_File,
               "   " & FileName &
               " : aliased Mcc.Gui.Container.Window.Main_Window;");
         else
            Ada.Text_IO.Put_Line(Spec_File,
               "   " & FileName &
               " : aliased Mcc.Gui.Container.Window.SubWindow;");
         end if;
         Ada.Text_IO.New_Line(Spec_File);

         Ada.Text_IO.Put_Line(Spec_File,
            "   procedure Generate_Window;");
         Ada.Text_IO.New_Line(File => Spec_File);
         Ada.Text_IO.Put_Line(Spec_File,
         "end " & Filename & ";");
         Ada.Text_IO.Close(File => Spec_File);
      end Generate_Spec;

      procedure Generate_Body(Filename : in String;
         Widget_List : in Gui.Widget.Widget_List) is
         Body_File : Ada.Text_IO.File_Type;
         Current_Widget_Position : Gui.Widget.Widget_Pointer;
         NoMenu                  : Boolean;
      begin
         NoMenu := Gui.Menu.Menu_List_Package.IsEmpty(Window.Menu);
         Ada.Text_IO.Create(
            File => Body_File,
            Name => Filename & ".adb",
            Mode => Ada.Text_IO.Out_File);
         Ada.Text_IO.Put_Line(Body_File,"-- Auto generated by RAPID");
         Ada.Text_IO.Put_Line(Body_File,"-- By: Martin C. Carlisle and W. Blair Watkinson II");
         Ada.Text_IO.New_Line(Body_File);

         -- context clauses for widget actions
         Current_Widget_Position := Gui.Widget.Widget_List_Package.First(Widget_List);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(L => Widget_List,
            P => Current_Widget_Position) loop
            Gui.Widget.Generate_Action_Context_Clause(
               Widget => Gui.Widget.Widget_List_Package.Retrieve(L => Widget_List,
                  P => Current_Widget_Position).all,
               File => Body_File);
            Gui.Widget.Widget_List_Package.GoAhead(L => Widget_List,
               P => Current_Widget_Position);
         end loop;
         Ada.Text_IO.New_Line(Body_File);

         -- context clauses for menu actions
         Gui.Menu.Generate_Action_Context_Clause(File => Body_File,
            Menu => Window.Menu);
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "with Mcc.Gui.Fonts;");
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "package body " & FileName & " is");
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "   procedure Generate_Window is");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "   begin");
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "      Mcc.Gui.Container.Window.Create(");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "         Obj    => " & FileName & ",");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "         X      => " & "0" & ",");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "         Y      => " & "0" & ",");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "         Width  =>" & Integer'Image(Window.Width) & ",");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "         Height =>" & Integer'Image(Window.Height) & ");");
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "      Mcc.Gui.Container.Window.Set_Title(");
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "         Obj   => " & FileName & ",");
         if Window.Title /= null then
            Ada.Text_IO.Put_Line(File => Body_File,
               Item => "         Title => " & """" & Window.Title.all & """" & ");");
         else
            Ada.Text_IO.Put_Line(File => Body_File,
               Item => "         Title => " & """" & FileName(FileName'First..FileName'Last-7) & """" & ");");
         end if;
         Ada.Text_IO.New_Line(Body_File);

         if not NoMenu then
            Ada.Text_IO.Put_Line(File => Body_File,
               Item => "      Mcc.Gui.Menu.Create(");
            Ada.Text_IO.Put_Line(File => Body_File,
               Item => "         Obj    => " & FileName(FileName'First..FileName'Last-7) & "_Menu,");
            Ada.Text_IO.Put_Line(File => Body_File,
               Item => "         Window => " & FileName & ");");
            Ada.Text_IO.New_Line(Body_File);
            Gui.Menu.Generate_Menu_Creation(
               Menu   => Window.Menu,
               File   => Body_File,
               Parent => FileName(FileName'First..FileName'Last-7) & "_Menu");
         end if;
         Ada.Text_IO.New_Line(Body_File);

         Current_Widget_Position := Gui.Widget.Widget_List_Package.First(Widget_List);
         while not Gui.Widget.Widget_List_Package.IsPastEnd(L => Widget_List,
            P => Current_Widget_Position) loop
            Gui.Widget.Generate_Widget_Creation(
               Widget      => Gui.Widget.Widget_List_Package.Retrieve(
                  L => Widget_List,
                  P => Current_Widget_Position).all,
               File        => Body_File,
               Window_Name => FileName);
            Gui.Widget.Generate_Widget_Font(
               Widget => Gui.Widget.Widget_List_Package.Retrieve(
                  L => Widget_List,
                  P => Current_Widget_Position).all,
               File        => Body_File);
            Gui.Widget.Widget_List_Package.GoAhead(L => Widget_List,
               P => Current_Widget_Position);
         end loop;
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "   end Generate_Window;");
         Ada.Text_IO.New_Line(Body_File);

         Ada.Text_IO.New_Line(Body_File);
         Ada.Text_IO.Put_Line(File => Body_File,
            Item => "end " & FileName & ";");
         Ada.Text_IO.Close(Body_File);
      end Generate_Body;

      Filename    : String :=
         Ada.Characters.Handling.To_Lower(
            Convert_Window_Name(Window.Window_Name.all) & "_window");
   begin -- Generate_Window

      Generate_Spec(Filename, Window.Widget_List);
      Generate_Body(Filename, Window.Widget_List);

   end Generate_Window;

   --------------------------------------------------------------
   -- changes the filename for the Gui_Window
   --
   -- 1) Free the filename string
   -- 2) allocate new string
   -- 3) change title of window using filename w/o directory
   --------------------------------------------------------------
   procedure Change_Filename(
         Window : in out Gui_Window;
         Filename : in String) is
      procedure Free is new Ada.Unchecked_Deallocation(String,String_Pointer);
   begin
      Free(Window.Filename);
      Window.Filename := new String'(Filename);
      Mcc.Gui.Container.Window.Set_Title(
         Obj   => Main_Window.Main_Window,
         Title => Window.Filename.all);
   end Change_Filename;

end Gui.Window;
