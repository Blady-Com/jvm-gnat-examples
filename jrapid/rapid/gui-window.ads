---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  GUI-WINDOW.ADS
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
-- A Window consists of:
--   a filename (of the .GUI file)
--   a window name (used when code is generated)
--   a width and height
--   a list of widgets
--   a menubar
--   a title (added using suggestion by Dmitri Anisimkov 2/22/99)
--
-- A window knows how to:
--   Create itself (set fields of record)
--   Resize itself (both on display and in record)
--   Display itself
--   Read its widgets, menus, etc. from a file
--   Write itself to a file
--   Generate code for itself (including widgets, menu)
--   Change its filename
---------------------------------------------------------------
with Gui.Menu;
with Gui.Widget;
with Mcc.Gui.Container.Frame;
with Mcc.Gui.Menu;

package Gui.Window is

   type GUI_Window is tagged limited record
      Filename    : String_Pointer;
      Window_Name : String_Pointer;
      Title       : String_Pointer;
      Width,Height: Integer;
      Widget_List : Gui.Widget.Widget_List;
      Menu        : Gui.Menu.Menu_Pointer;
      Menu_Area   : Mcc.Gui.Container.Frame.Frame;
      Display_Area: aliased Mcc.Gui.Container.Frame.Frame;
      Resize_Frame: aliased Mcc.Gui.Container.Frame.Frame;
      Menu_Display: Mcc.Gui.Menu.Window_Menu;
   end record;
   type Window_Pointer is access all GUI_Window;

   --------------------------------------------------------
   -- procedure Create_Windows
   --
   -- Initialize a GUI_Window
   --
   -- Fill in the fields of the Window
   --------------------------------------------------------
   procedure Create_Window(Window : in out Gui_Window; Filename : in String;
      Width       : in Integer;
      Height      : in Integer;
      Title       : in String;
      Window_Name : in String);

   --------------------------------------------------------
   -- procedure Resize_Window
   --
   -- resize the GUI_Window
   --
   -- update Window record and move window
   --------------------------------------------------------
   procedure Resize_Window(
      Window : in out Gui_Window;
      Width  : in Integer;
      Height : in Integer);

   --------------------------------------------------------
   -- display the GUI Window
   --
   -- Display Window, its menu and widgets
   --------------------------------------------------------
   procedure Display_Window(Window : in out Gui_Window);

   --------------------------------------------------------
   -- procedure Resize_Window
   --
   -- dynamic resize using handle
   --
   -- when Window is resized using the handle, call this 
   -- instead-- it just computes width and height and
   -- calls the other
   --------------------------------------------------------
   procedure Resize_Window(
      Window : in out Gui_Window;
      startx : in Integer;
      starty : in Integer;
      endx   : in Integer;
      endy   : in Integer);

   --------------------------------------------------------
   -- procedure Undisplay_Window
   --
   -- destroy the GUI Window
   --------------------------------------------------------
   procedure Undisplay_Window(Window : in Gui_Window);

   --------------------------------------------------------
   -- procedure Read_Window
   --
   -- reads information from a (.GUI) file into GUI_Window
   -- raises Bad_File if GUI file is ill-formed
   --------------------------------------------------------
   Bad_File : exception;
   procedure Read_Window(Window : in out Gui_Window; Filename : in String);

   --------------------------------------------------------
   -- procedure Write_Window
   --
   -- Writes information to a (.GUI) file from GUI_Window
   -- filename is stored in Window
   -- also writes the widgets and menu
   --------------------------------------------------------
   procedure Write_Window(Window : in Gui_Window);

   --------------------------------------------------------
   -- procedure Generate_Window
   --
   -- Generate Ada code for this window.
   -- also generates code for Widgets and menu
   --------------------------------------------------------
   procedure Generate_Window(Window : in Gui_Window);

   --------------------------------------------------------
   -- procedure Change_Filename
   --
   -- changes the filename for the Gui_Window
   -- also updates display
   --------------------------------------------------------
   procedure Change_Filename(
      Window : in out Gui_Window;
      Filename : in String);

end Gui.Window;