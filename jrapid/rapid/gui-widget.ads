---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  GUI-WIDGET.ADS 
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
-- Change log:
-- 08/20/98 (mcc) : incorporate workaround for GNAT 3.10p bug 
--                  workaround contributed by Frederic Abiven
---------------------------------------------------------------
-- This starts the Gui-Widget hierarchy.  Most widgets
-- will be a direct descendant of GUI_Widget
--
-- Most other packages will use Widget_Access, which can
-- point to any widget in the hierarchy.  Also the widget
-- list packages are used in Gui-Window to keep track of
-- all the widgets in a window.
--
-- ATTRIBUTES:
-- All widgets must have a location, size and name (see below)
--
-- METHODS:
-- All widgets know how to:
-- 1) Set their values by reading from a file
-- 2) Write themselves to a file
-- 3) Display and undisplay themselves
-- 4) Generate code for themselves
-- 5) Highlight and unhighlight themselves
-- 6) Move themselves (with highlight)
-- 7) Display a properties dialog and 
--    change their values based on the result
---------------------------------------------------------------
with ada.text_io;
--with Lists_Generic,Lists_Generic.Key;
with Lists_Generic_2Keys;
with Mcc.Gui.Container.Window;
with Mcc.Gui.Widget.Text_Entry;
with Mcc.Gui.Widget.Label;
with Mcc.Gui.Widget.Rectangle;
with Mcc.Gui.Fonts;

package Gui.Widget is

   type GUI_Widget;
   type Widget_Access is access all GUI_Widget'Class;

   -- Note this is abstract, so GUI_Widget can't be
   -- declared directly, it is only holding
   -- values shared by all its children
   type GUI_Widget is abstract tagged record
      x,y,Width,Height : Integer;
      Name         : String_Pointer := NULL;
      -- consider Font_Family, etc., only if Have_Font is true
      Have_Font    : Boolean := False;
      Font_Family  : Mcc.Gui.Fonts.Font_Family := Mcc.Gui.Fonts.Serif;
      Font_Size    : Mcc.Gui.Fonts.Font_Size := 10;
      Font_Style   : Mcc.Gui.Fonts.Font_Style := Mcc.Gui.Fonts.Plain;
      -- used to display the selected font
      Font_Label   : Mcc.Gui.Widget.Label.Label_Pointer;
      Style_Label  : Mcc.Gui.Widget.Label.Label_Pointer;
      -- used to read common entries
      Name_Entry   : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      X_Entry      : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Y_Entry      : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Width_Entry  : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Height_Entry : Mcc.Gui.Widget.Text_Entry.Text_Entry_Pointer;
      Properties   : Mcc.Gui.Container.Window.Window_Pointer;
      The_Widget   : Mcc.Gui.Widget.Widget_Pointer;
      -- used to track if highlighted
      Is_Selected  : Boolean := False;
      -- used if the widget is highlighted
      NW_Handle : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      N_Handle  : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      NE_Handle : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      E_Handle  : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      SE_Handle : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      S_Handle  : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      SW_Handle : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
      W_Handle  : aliased Mcc.Gui.Widget.Rectangle.Rectangle;
   end record;

   -------------------------------------------------
   -- procedure Read_Widget
   --
   -- reads information from a (.GUI) file into 
   -- a previously created GUI_Widget,
   -- assumes keyword already read.
   --
   -- For example, if we have:
   --   LABEL label1 0 5 110 20 "Widget name"
   --
   -- then LABEL was read and a LABEL widget created
   -- (this happens in widget_io), then this gets
   -- called to read the rest and fill in the fields
   -- of the LABEL
   -------------------------------------------------
   procedure Read_Widget(Widget : in out Gui_Widget;
      File        : in Ada.Text_IO.File_Type);

   -------------------------------------------------
   -- procedure Write_Widget
   --
   --
   -- Writes information from this Widget to a (.GUI) file 
   -- This includes the entire line including the
   -- type of widget (see Read_Widget above)
   -------------------------------------------------
   procedure Write_Widget(Widget : in Gui_Widget;
      File : in Ada.Text_IO.File_Type);

   -------------------------------------------------
   -- procedure Generate_Action_Context_Clause
   --
   -- As before, not abstract, but null, and should be overridden for
   -- each widget containing action(s).
   --
   -- Since an action is a fully-qualified Ada procedure
   -- this will create with clause(s) for appropriate
   -- packages referenced in the procedure call
   -- (right now only does 1st)
   --
   -- called from Gui.Window.Generate_Window
   -------------------------------------------------
   -- wbw 6/6/99
   procedure Generate_Action_Context_Clause(Widget : in Gui_Widget;
      File : in Ada.Text_IO.File_Type);

   -------------------------------------------------
   -- procedure Generate_Widget_Context_Clauses
   --
   -- Abstract, requiring each widget to create this
   -- procedure
   --
   -- Since a widget is a fully-qualified Ada object,
   -- and includes methods and attributes,
   -- this will create the with clause for the 
   -- appropriate packages referenced by the Ada
   -- object
   --
   -- called from Gui.Window.Generate_Window
   -------------------------------------------------      
   -- wbw 6/6/99
   procedure Generate_Widget_Context_Clause(Widget : in Gui_Widget;
      File : in Ada.Text_IO.File_Type) is abstract;

   -------------------------------------------------
   -- procedure Generate_Widget_Declaration
   --
   -- Since a widget is a variable which must be
   -- declared as an Ada object or type, this
   -- will create the appropriate declarations for
   -- each widget
   --
   -- called from Gui.Window.Generate_Window
   -------------------------------------------------      
   -- wbw 6/6/99
   procedure Generate_Widget_Declaration(Widget : in Gui_Widget;
      File : in Ada.Text_IO.File_Type) is abstract;

   -- wbw 5/10/99
   procedure Generate_Widget_Creation(Widget : in Gui_Widget;
      File        : in Ada.Text_IO.File_Type;
      Window_Name : in String) is abstract;

   -------------------------------------------------
   -- procedure Generate_Widget_Font
   --
   -- if widget has a font, create it and then set
   -- it for the widget
   -------------------------------------------------
   procedure Generate_Widget_Font(Widget : in Gui_Widget;
      File        : in Ada.Text_IO.File_Type);
      
   -------------------------------------------------
   -- procedure Display_Widget
   --
   -- display the widget to user's view of window
   -- being edited
   -------------------------------------------------
   procedure Display_Widget(Widget : in out Gui_Widget;
      Container : in out Mcc.Gui.Container.Container'Class);

   -------------------------------------------------
   -- procedure Undisplay_Widget
   --
   -- remove the widget from user's view of window
   -- being edited
   -------------------------------------------------
   procedure Undisplay_Widget(Widget : in Gui_Widget);

   -------------------------------------------------
   -- procedure Highlight
   --
   -- highlight a selected widget, that is,
   -- display the sizer buttons around it
   -- 
   -- see Subwindow_Actions.Select_widget for a usage
   -------------------------------------------------
   procedure Highlight(Widget : in out Gui_Widget);

   -------------------------------------------------
   -- procedure Move_Highlight
   --
   -- if user moves the widget, move the displayed
   -- resizing buttons
   -------------------------------------------------
   procedure Move_Highlight(Widget : in out Gui_Widget);

   -------------------------------------------------
   -- procedure Unhighlight
   --
   -- destroy resizing buttons (useful if user
   -- selects a different widget, for example)
   -------------------------------------------------
   procedure Unhighlight(Widget : in Gui_Widget);

   -------------------------------------------------
   -- procedure Set_Properties
   --
   -- run properties dialog with properties from widget
   -- (when user right clicks on widget)
   -- called from Subwindow_Actions.Modify_Widget
   -------------------------------------------------
   procedure Set_Properties(Widget : in out Gui_Widget);

   -------------------------------------------------
   -- procedure Apply_Properties
   --
   -- called when OK pushed in properties dialog
   -------------------------------------------------
   procedure Apply_Properties(Widget : in out Gui_Widget);

   -------------------------------------------------
   -- procedure Check_Properties
   --
   -- looks at entries in dialog, and sees if they are 
   -- ok.  If not, highlights the incorrect entry.
   -------------------------------------------------
   procedure Check_Properties(Widget : in out Gui_Widget;
      Ok : out Boolean);

   -------------------------------------------------
   -- procedure Close_Properties
   --
   -- destroys dialog.  Called after Check_Properties
   -- if user pushes "OK", or directly on "Cancel"
   -------------------------------------------------
   procedure Close_Properties(Widget : in Gui_Widget);

   -------------------------------------------------
   -- function Get_Name
   --
   -- simple selector that returns name stored in Widget.Name
   -- GUI_Widget should probably be private, so this
   -- would enable us to find out the name w/o knowing
   -- the representation.
   -------------------------------------------------
   function Get_Name(Widget : in Widget_Access) return String;

   -------------------------------------------------
   -- procedure Set_Location
   --
   -- set x,y,width and height for a widget
   -- would assist in making private (still too many refs
   -- in subwindow_actions to do this)
   -------------------------------------------------
   procedure Set_Location(Widget : in Widget_Access;
      x,y,Width,Height : in Integer);

--   package Widget_List_Package is new Lists_Generic(Widget_Access);
--   package Widget_Key_List_Package is new Widget_List_Package.Key(
--      GetKey => Get_Name);
   -- GNAT 3.10p doesn't correctly manage nested scope
   -- workaround by Frederic Abiven

   -------------------------------------------------
   -- function Get_The_Widget
   -------------------------------------------------
   function Get_The_Widget(Widget : in Widget_Access) 
      return Mcc.Gui.Widget.Widget_Pointer;

   package WK is new Lists_Generic_2Keys(
      Element  => Widget_Access,
      Getkey   => Get_Name,
      Key2type => Mcc.Gui.Widget.Widget_Pointer,
      Getkey2  => Get_The_Widget);
      
   package Widget_List_Package renames WK.List_Package;
   package Widget_Key_List_Package renames WK.List_Package_Key;
   package Widget_Key2_List_Package renames WK.List_Package_Key2;
   
   -- simple renamings to make things a bit shorter
   -- Don't confuse Widget_Pointer (a place in a widget list)
   -- with Widget_Access (a pointer to a widget)
   subtype Widget_Pointer is Widget_List_Package.Position;
   subtype Widget_List is Widget_List_Package.List;


end Gui.Widget;
