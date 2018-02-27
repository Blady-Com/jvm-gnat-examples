---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET.ADB
--  Description : Root of GUI Widget Hierarchy
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
with File_Helpers;
with Ada.Integer_Text_IO,Ada.Text_IO;
with Mcc.Common_Dialogs;
with Ada.Characters.Handling;
with State;
with Mcc.Gui.Widget.Label;
with Mcc.Gui.Container.Window;
with Subwindow_Actions;
with Ada.Exceptions;
with Mcc.Gui.Widget.Rectangle;
with Gui.Window;
with Font_Actions;

package body Gui.Widget is
   package Font_Family_Io is new Ada.Text_Io.Enumeration_Io(
      Mcc.Gui.Fonts.Font_Family);
   package Font_Style_Io is new Ada.Text_Io.Enumeration_Io(
      Mcc.Gui.Fonts.Font_Style);

   Handle_Size : constant Integer := 6;
   -- size of handles that are created if a widget is selected
   -- Should be even
   -----------------------------------------------------
   -- File is already open and widget type read
   -- read name, x, y, width and height
   -- used in child types to read these common
   -- attributes
   -----------------------------------------------------
   procedure Read_Widget(Widget : in out GUI_Widget;
         File        : in Ada.Text_IO.File_Type) is
      Word   : Word_Type;
      Last   : Natural;
      Item   : Character := ' ';
      At_End : Boolean   := False;
   begin -- Read_Widget
      File_Helpers.Get_String(File,Word,Last);
      Widget.Name := new String'(Word(1..Last));

      Ada.Integer_Text_IO.Get(File,Widget.X);
      Ada.Integer_Text_IO.Get(File,Widget.Y);
      Ada.Integer_Text_IO.Get(File,Widget.Width);
      Ada.Integer_Text_IO.Get(File,Widget.Height);
      
      -- we use an '!' to indicate there is font information
      -- since the enumeration get was consuming the word
      -- false from listboxes
      if not Ada.Text_Io.End_Of_Line(File) then
         while Item = ' ' and not At_End loop
            Ada.Text_Io.Look_Ahead(File,Item,At_End);
            if Item = ' ' and not At_End then
               Ada.Text_Io.Get(File,Item);
            end if;
         end loop;
      end if;

      if Item = '!' then
         Ada.Text_Io.Get(File,Item); -- consume '!'
         
         Font_Family_Io.Get(File,Widget.Font_Family);
         
         -- if no exception, read font size and style
         Ada.Integer_Text_Io.Get(File,Widget.Font_Size);
         Font_Style_Io.Get(File,Widget.Font_Style);
         Widget.Have_Font := True;
      else
         Widget.Have_Font := False;
      end if;
   end Read_Widget;

   -----------------------------------------------------
   -- write out widget type, name, x, y, width and
   -- height to already created file.
   -- used in child types to write these common
   -- attributes
   -----------------------------------------------------
   procedure Write_Widget(Widget : in GUI_Widget;
         File : in Ada.Text_IO.File_Type) is
      use type Mcc.Gui.Fonts.Font_Family;
   begin -- Write_Widget
      Ada.Text_IO.Put(File,Widget.Name.all);
      Ada.Text_IO.Put(File,' ');
      Ada.Integer_Text_IO.Put(File,Widget.X,0);
      Ada.Text_IO.Put(File,' ');
      Ada.Integer_Text_IO.Put(File,Widget.Y,0);
      Ada.Text_IO.Put(File,' ');
      Ada.Integer_Text_IO.Put(File,Widget.Width,0);
      Ada.Text_IO.Put(File,' ');
      Ada.Integer_Text_IO.Put(File,Widget.Height,0);
      if Widget.Have_Font then
         Ada.Text_IO.Put(File," ! ");
         Font_Family_Io.Put(File,Widget.Font_Family);
         Ada.Text_IO.Put(File,' ');
         Ada.Integer_Text_IO.Put(File,Widget.Font_Size,0);
         Ada.Text_IO.Put(File,' ');
         Font_Style_Io.Put(File,Widget.Font_Style);
      end if;
   end Write_Widget;


   -----------------------------------------------------
   -- this is not abstract, but null, and should be overridden for
   -- each widget containing action(s).
   -----------------------------------------------------
   procedure Generate_Action_Context_Clause(Widget : in Gui_Widget;
         File : in Ada.Text_IO.File_Type) is
   begin -- Generate_Context_Clause
      null;
   end Generate_Action_Context_Clause;
   
   Start_X, Start_Y : Integer;
   procedure Widget_Mouse_Listener(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
      use Mcc.Gui;
   begin
      if Event.Button = Right and Event.Action = Press then
         Subwindow_Actions.Modify_Widget(
            Mcc.Gui.Widget.Widget'Class(Obj));
      elsif Event.Button = Left and Event.Action = Press then
         Mcc.Gui.Set_Cursor(
            Obj    => Mcc.Gui.Sized_Object(Obj),
            Cursor => Mcc.Gui.Move_Cursor);
         Subwindow_Actions.Select_Widget(
            Mcc.Gui.Widget.Widget'Class(Obj));
         Start_X := Event.X;
         Start_Y := Event.Y;
      elsif Event.Button = Left and Event.Action = Move then
         Subwindow_Actions.Move_Widget(
            Mcc.Gui.Widget.Widget'Class(Obj),
            Start_X,
            Start_Y,
            Event.X,
            Event.Y);
      elsif Event.Button = Left and Event.Action = Release then
         Mcc.Gui.Set_Cursor(
            Obj    => Mcc.Gui.Sized_Object(Obj),
            Cursor => Mcc.Gui.Default_Cursor);
      end if;
   end Widget_Mouse_Listener;

   procedure Resize_Mouse_Listener_All(
      Handle: in String;
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
      use Mcc.Gui;
   begin
      if Event.Button = Left and Event.Action = Press then
         Start_X := Event.X;
         Start_Y := Event.Y;
      elsif Event.Button = Left and Event.Action = Move then
         Subwindow_Actions.Resize_Widget(
            Handle,Start_X,Start_Y,Event.X,Event.Y);
      end if;
   end Resize_Mouse_Listener_All;
   procedure Resize_Mouse_Listener_NW(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("nw",Obj,Event);
   end Resize_Mouse_Listener_NW;
   procedure Resize_Mouse_Listener_N(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("n",Obj,Event);
   end Resize_Mouse_Listener_N;
   procedure Resize_Mouse_Listener_NE(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("ne",Obj,Event);
   end Resize_Mouse_Listener_NE;
   procedure Resize_Mouse_Listener_E(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("e",Obj,Event);
   end Resize_Mouse_Listener_E;
   procedure Resize_Mouse_Listener_SE(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("se",Obj,Event);
   end Resize_Mouse_Listener_SE;
   procedure Resize_Mouse_Listener_S(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("s",Obj,Event);
   end Resize_Mouse_Listener_S;
   procedure Resize_Mouse_Listener_SW(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("sw",Obj,Event);
   end Resize_Mouse_Listener_SW;
   procedure Resize_Mouse_Listener_W(
      Obj   : in out Mcc.Gui.Sized_Object'Class;
      Event : in     Mcc.Gui.Mouse_Event) is
   begin
      Resize_Mouse_Listener_All("w",Obj,Event);
   end Resize_Mouse_Listener_W;
   
   -----------------------------------------------------
   -- display the widget to a window
   --
   -- place the widget at its proper location
   -- (will be called after it is created by a
   --  method of the same name in a child type)
   -- Also binds the right mouse button to modify_widget
   --            the left mouse button to select_widget
   --            moving left button to moving widget
   -- Cursor is changed while left mouse is depressed on 
   -- widget
   -----------------------------------------------------
   procedure Display_Widget(Widget : in out Gui_Widget;
         Container : in out Mcc.Gui.Container.Container'Class) is
      The_Font : Mcc.Gui.Fonts.Font;
   begin
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Mcc.Gui.Sized_Object_Pointer(Widget.The_Widget),
         Listener => Widget_Mouse_Listener'access);

      if Widget.Have_Font then
         Mcc.Gui.Fonts.Create(
            Obj    => The_Font,
            Family => Widget.Font_Family,
            Size   => Widget.Font_Size,
            Style  => Widget.Font_Style);
         Mcc.Gui.Fonts.Set_Font(
            Obj      => Mcc.Gui.Sized_Object(Widget.The_Widget.all),
            New_Font => The_Font);
      end if;
         
   exception
      when others =>
         Mcc.Common_Dialogs.OK_Box("Unable to display: " &
            Widget.Name.all & ASCII.LF);
         Undisplay_Widget(Widget);
   end Display_Widget;

   -----------------------------------------------------
   -- highlight a selected widget
   --
   -- calls a create_handles (Tcl) procedure created
   -- in the package begin/end block below
   -- then binds dragging on each of the 8 resize handles
   -- (nw,ne,sw,se,nn,ss,ee,ww)
   -----------------------------------------------------
   procedure Highlight(Widget : in out Gui_Widget) is
      Window : Gui.Window.Window_Pointer := State.Get_Current_Window;
   begin
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.NW_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X-Handle_Size,
         Y      => Widget.Y-Handle_Size,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.NE_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X+Widget.Width,
         Y      => Widget.Y-Handle_Size,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.N_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X+Widget.Width/2-Handle_Size/2,
         Y      => Widget.Y-Handle_Size,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.SW_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X-Handle_Size,
         Y      => Widget.Y+Widget.Height,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.SE_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X+Widget.Width,
         Y      => Widget.Y+Widget.Height,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.S_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X+Widget.Width/2-Handle_Size/2,
         Y      => Widget.Y+Widget.Height,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.W_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X-Handle_Size,
         Y      => Widget.Y+Widget.Height/2-Handle_Size/2,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Widget.Rectangle.Create(
         Obj    => Widget.E_Handle,
         Parent => Window.Display_Area,
         X      => Widget.X+Widget.Width,
         Y      => Widget.Y+Widget.Height/2-Handle_Size/2,
         Width  => Handle_Size,
         Height => Handle_Size);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.NW_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_NW'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.N_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_N'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.NE_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_NE'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.E_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_E'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.SE_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_SE'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.S_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_S'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.SW_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_SW'access);
      Mcc.Gui.Set_Mouse_Listener(
         Obj      => Widget.W_Handle'unchecked_access,
         Listener => Resize_Mouse_Listener_W'access);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.NW_Handle),
         Cursor => Mcc.Gui.Resize_NW);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.N_Handle),
         Cursor => Mcc.Gui.Resize_N);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.NE_Handle),
         Cursor => Mcc.Gui.Resize_NE);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.E_Handle),
         Cursor => Mcc.Gui.Resize_E);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.SE_Handle),
         Cursor => Mcc.Gui.Resize_SE);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.S_Handle),
         Cursor => Mcc.Gui.Resize_S);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.SW_Handle),
         Cursor => Mcc.Gui.Resize_SW);
      Mcc.Gui.Set_Cursor(
         Obj    => Mcc.Gui.Sized_Object(Widget.W_Handle),
         Cursor => Mcc.Gui.Resize_W);

   exception
      when e:others => Mcc.Common_Dialogs.OK_Box(
         "Highlight: " & Ada.Exceptions.Exception_Information(E));
   end Highlight;

   -----------------------------------------------------
   -- highlight a selected widget
   --
   -- done by calling Tcl procedure move_handles
   -- defined below in the package begin/end block
   -----------------------------------------------------
   procedure Move_Highlight(Widget : in out Gui_Widget) is
   begin
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.NW_Handle,
         X      => Widget.X-Handle_Size,
         Y      => Widget.Y-Handle_Size);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.NE_Handle,
         X      => Widget.X+Widget.Width,
         Y      => Widget.Y-Handle_Size);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.N_Handle,
         X      => Widget.X+Widget.Width/2-Handle_Size/2,
         Y      => Widget.Y-Handle_Size);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.SW_Handle,
         X      => Widget.X-Handle_Size,
         Y      => Widget.Y+Widget.Height);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.SE_Handle,
         X      => Widget.X+Widget.Width,
         Y      => Widget.Y+Widget.Height);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.S_Handle,
         X      => Widget.X+Widget.Width/2-Handle_Size/2,
         Y      => Widget.Y+Widget.Height);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.W_Handle,
         X      => Widget.X-Handle_Size,
         Y      => Widget.Y+Widget.Height/2-Handle_Size/2);
      Mcc.Gui.Widget.Rectangle.Move(
         Obj    => Widget.E_Handle,
         X      => Widget.X+Widget.Width,
         Y      => Widget.Y+Widget.Height/2-Handle_Size/2);
   exception
      when others => null;
   end Move_Highlight;

   -----------------------------------------------------
   -- calls the destroy_handles (Tcl) procedure
   -- defined below in package begin/end block
   -----------------------------------------------------
   procedure Unhighlight(Widget : in Gui_Widget) is
   begin
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.NW_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.N_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.NE_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.E_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.SE_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.S_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.SW_Handle);
      Mcc.Gui.Widget.Rectangle.Destroy(Widget.W_Handle);
   exception
      when others => null;
   end Unhighlight;

   -----------------------------------------------------
   -- Tell Tcl to destroy the widget!
   -----------------------------------------------------
   procedure Undisplay_Widget(Widget : in Gui_Widget) is
   begin
      Mcc.Gui.Destroy(Mcc.Gui.Object(Widget.The_Widget.all));      
   exception
      when others => null;
   end Undisplay_Widget;

   -----------------------------------------------------
   -- Algorithm:
   -- 1) Get dialog name
   -- 2) Change dialog appearance to transient
   -- 3) If widget has name (won't if just created), 
   --    set entry1
   -- 4) set entries2-5 to x,y,width,height
   -- 5) Bind clicking window close to 
   --    delete_properties_action (see tcl_bindings.txt)
   --    this calls Subwindow_Actions.Cancel_Properties_Dialog
   -- 6) Put cursor in entry1
   -- Note: other properties set in method of same name
   -- in child class
   -----------------------------------------------------
   procedure Set_Properties(Widget : in out Gui_Widget) is
      use type Mcc.Gui.Widget.Label.Label_Pointer;
   begin

      if Widget.Name /= null then
         Mcc.Gui.Widget.Text_Entry.Set_Text(
            Obj  => Widget.Name_Entry.all,
            Text => Widget.Name.all);
      end if;

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.X_Entry.all,
         Text => Widget.X);
         
      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Y_Entry.all,
         Text => Widget.Y);

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Width_Entry.all,
         Text => Widget.Width);

      Mcc.Gui.Widget.Text_Entry.Set_Text(
         Obj  => Widget.Height_Entry.all,
         Text => Widget.Height);

      Mcc.Gui.Container.Window.Set_Close_Handler(
         Obj     => Widget.Properties,
         Handler => Subwindow_Actions.Cancel_Properties_Dialog'access);

      Mcc.Gui.Widget.Text_Entry.Highlight(
         Obj   => Widget.Name_Entry.all,
         Start => 0,
         Stop  => 0);

      if Widget.Font_Label /= null then
         if Widget.Have_Font then
            Mcc.Gui.Widget.Label.Set_Text(
               Obj  => Widget.Font_Label.all,
               Text => "Font: " & Font_Actions.Display_Enum(
                  Mcc.Gui.Fonts.Font_Family'image(
                     Widget.Font_Family)));
            Mcc.Gui.Widget.Label.Set_Text(
               Obj  => Widget.Style_Label.all,
               Text => Font_Actions.Display_Enum(
                  Mcc.Gui.Fonts.Font_Style'image(
                     Widget.Font_Style)) &
                  Mcc.Gui.Fonts.Font_Size'image(Widget.Font_Size) &
                  " pt");
         else
            Mcc.Gui.Widget.Label.Set_Text(
               Obj  => Widget.Font_Label.all,
               Text => "Font:");
            Mcc.Gui.Widget.Label.Set_Text(
               Obj  => Widget.Style_Label.all,
               Text => "Default");
         end if;
      end if;
   end Set_Properties;

   -----------------------------------------------------
   -- Algorithm:
   -- 1) Get dialog name
   -- 2) Read Widget_Name into Word
   -- 3) Read x,y,width,height directly into window
   --    3a) set to -99 if invalid text 
   -- Note: other properties applied in method of same
   -- name in child class
   -- Widget name can't begin upper case-- fix this!
   -----------------------------------------------------
   procedure Apply_Properties(Widget : in out Gui_Widget) is
   begin
      declare
         Name : String := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Name_Entry.all);
      begin
         Widget.Name := new String'(Name);
      end;

      begin
         Widget.X := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.X_Entry.all);
      exception
         when others => Widget.X := -99;
      end;

      begin
         Widget.Y := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Y_Entry.all);
      exception
         when others => Widget.Y := -99;
      end;

      begin
         Widget.Width := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Width_Entry.all);
      exception
         when others => Widget.Width := -99;
      end;

      begin
         Widget.Height := Mcc.Gui.Widget.Text_Entry.Get_Text(
            Widget.Height_Entry.all);
      exception
         when others => Widget.Height := -99;
      end;

   end Apply_Properties;

   -----------------------------------------------------
   -- Check properties common to all widgets.  Others
   -- checked from method of same name in child class
   --
   -- Make sure name isn't null 
   -- Make sure name is alphanumeric and has no "_"
   -- If not OK, highlight entry1
   --
   -- Note we stop checking as soon as we find something
   -- that isn't OK.
   --
   -- Check x,y,width,height to make sure they are positive
   -- and highlight if not
   -----------------------------------------------------
   procedure Check_Properties(Widget : in out Gui_Widget;
      Ok : out Boolean) is
   begin
      Ok := Widget.Name /= null and then Widget.name.all'length >= 1;

      if Ok then
         for i in Widget.Name.all'range loop
            if not Ada.Characters.Handling.Is_Alphanumeric(
               Widget.Name.all(i)) and then
               not (Widget.Name.all(i) = '_') then
               Ok := False;
            end if;
         end loop;
      end if;

      if not Ok then
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Name_Entry.all);
      end if;

      if Ok and then Widget.X < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.X_Entry.all);
      end if;

      if Ok and then Widget.Y < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Y_Entry.all);
      end if;

      if Ok and then Widget.Width < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Width_Entry.all);
      end if;

      if Ok and then Widget.Height < 0 then
         Ok := False;
         Mcc.Gui.Widget.Text_Entry.Highlight(
            Widget.Height_Entry.all);
      end if;
   end Check_Properties;

   -----------------------------------------------------
   -- Destroy the dialog.  Note that we don't have one
   -- running anymore. (Can only have 1 at a time since
   -- the dialog names are reused).
   -----------------------------------------------------
   procedure Close_Properties(Widget : in Gui_Widget) is
   begin
      Mcc.Gui.Destroy(Mcc.Gui.Object(Widget.Properties.all));
      State.Set_Dialog_Running(False);
   end Close_Properties;

   -----------------------------------------------------
   -- return the name
   -----------------------------------------------------
   function Get_Name(Widget : in Widget_Access) return String is
   begin
      return Widget.Name.all;
   end Get_Name;

   -------------------------------------------------
   -- function Get_The_Widget
   -------------------------------------------------
   function Get_The_Widget(Widget : in Widget_Access) 
      return Mcc.Gui.Widget.Widget_Pointer is
   begin
      return Widget.The_Widget;
   end Get_The_Widget;

   -------------------------------------------------
   -- procedure Set_Location
   --
   -- set x,y,width and height for a widget
   -------------------------------------------------
   procedure Set_Location(Widget : in Widget_Access;
      x,y,Width,Height : in Integer) is
   begin
      Widget.x := x;
      Widget.y := y;
      Widget.Width := Width;
      Widget.Height := Height;
   end Set_Location;

   procedure Generate_Widget_Font(Widget : in Gui_Widget;
      File        : in Ada.Text_IO.File_Type) is
   begin
      if Widget.Have_Font then
         Ada.Text_IO.Put_Line(File,"      declare");
         Ada.Text_IO.Put_Line(File,"         use Mcc.Gui.Fonts;");
         Ada.Text_IO.Put_Line(File,
            "         The_Font : Mcc.Gui.Fonts.Font;");
         Ada.Text_IO.Put_Line(File,"      begin");
         Ada.Text_IO.Put_Line(File,
            "         Mcc.Gui.Fonts.Create(");
         Ada.Text_IO.Put_Line(File,
            "            Obj    => The_Font,");
         Ada.Text_IO.Put_Line(File,
            "            Family => " & 
            Mcc.Gui.Fonts.Font_Family'image(Widget.Font_Family) & ",");
         Ada.Text_IO.Put_Line(File,
            "            Size   => " & 
            Mcc.Gui.Fonts.Font_Size'image(Widget.Font_Size) & ",");
         Ada.Text_IO.Put_Line(File,
            "            Style  => " &
            Mcc.Gui.Fonts.Font_Style'image(Widget.Font_Style) & ");");
         Ada.Text_IO.Put_Line(File,
            "         Mcc.Gui.Fonts.Set_Font(");
         Ada.Text_IO.Put_Line(File,
            "            Obj      => Mcc.Gui.Sized_Object(" &
            Widget.Name.all & "),");
         Ada.Text_IO.Put_Line(File,
            "            New_Font => The_Font);");
         Ada.Text_IO.Put_Line(File,"      end;");
      end if;
   end Generate_Widget_Font;

end Gui.Widget;
