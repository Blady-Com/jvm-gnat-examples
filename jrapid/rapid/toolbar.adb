---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer     
--                                                           
--  TOOLBAR.ADB 
--  Description : Manages the widget toolbar
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
-- 02/04/99 (mcc) : Implement suggestion by Dimitry Anisimkov
--                  to simplify addition of widgets to the toolbar
---------------------------------------------------------------
-- modified by C2C W. Blair Watkinson II on 16 Feb 99
-- added Scale_Str for Scale widget
with Mcc.Gui.Widget.Button.Picture;
use type Mcc.Gui.Widget.Button.Picture.Picture_Button;
with Main_Window;

package body Toolbar is
   Current_Widget_Type : Widget_Names := Textbutton;

   ----------------------------------------------------------------
   -- procedure Select_Widget
   --
   -- undepress old Current_Widget, then depress new Current_Widget
   -- update value of Current_Widget
   ----------------------------------------------------------------
   procedure Select_Widget(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      if Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         Main_Window.labelButton then 
         Select_Widget(Widget_Type => Label);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.textBButton then
         Select_Widget(Widget_Type => TextButton);   
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.pictureBButton then
         Select_Widget(Widget_Type => PictureButton);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.textEntryButton then
         Select_Widget(Widget_Type => Textentry);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.check_Button then
         Select_Widget(Widget_Type => Check_Button);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.radio_Button then
         Select_Widget(Widget_Type => Radio_Button);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.picture_select then
         Select_Widget(Widget_Type => Picture);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.listBoxButton then
         Select_Widget(Widget_Type => Listbox);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.scaleButton then
         Select_Widget(Widget_Type => Scale);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.ProgressButton then
         Select_Widget(Widget_Type => Progress);
      elsif Mcc.Gui.Widget.Button.Picture.Picture_Button(Obj) = 
         main_Window.dropdownButton then
         Select_Widget(Widget_Type => Dropdown);
      end if;
      Mcc.Gui.Widget.Button.Depress(Obj);
   end Select_Widget;   
  
   procedure Select_Widget(Widget_Type : Widget_Names) is
   begin -- Select_Widget
      case Current_Widget_Type is
         when Label =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.labelbutton);
         when Textbutton =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.textbbutton);
         when Picturebutton =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.picturebbutton);
         when Textentry =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.textentrybutton);
         when Check_Button =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.check_button);
         when Radio_button =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.radio_button);
         when Picture =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.picture_select);
         when Listbox =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.listboxbutton);
         when Scale =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.scalebutton);
         when Progress =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.ProgressButton);
         when Dropdown =>
            Mcc.Gui.Widget.Button.Picture.Release(
               Main_Window.DropdownButton);
      end case;
      Current_Widget_Type := Widget_Type;
   end Select_Widget;


   function Get_Selected_Widget return Widget_Names is
   begin -- Get_Selected_Widget
      return Current_Widget_Type;
   end Get_Selected_Widget;

   procedure Initialize_Toolbar is
   begin
      Current_Widget_Type := Label;
      Mcc.Gui.Widget.Button.Picture.Depress(
         Main_window.Labelbutton);
   end Initialize_Toolbar;

end Toolbar;