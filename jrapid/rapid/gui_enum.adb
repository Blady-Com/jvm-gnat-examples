-- modified by C2C W. Blair Watkinson II on 16 Feb 99
-- added when for "Scale"
-- added "with" for Scale
with Gui.Widget.Static;
with Gui.Widget.Button;
with Gui.Widget.Check_Button;
with Gui.Widget.Radio_Button;
with Gui.Widget.Picture;
with Gui.Widget.Listbox;
with Gui.Widget.Text;
with Gui.Widget.Progress;
with Gui.Widget.Scale;
with Gui.Widget.Dropdown;

package body Gui_Enum is

   ---------------------
   -- Allocate_Widget --
   ---------------------

   function Allocate_Widget
         (Widget_Type : in Widget_Keyword)
         return Gui.Widget.Widget_Access
         is
   begin
      case Widget_Type is
         when Label =>
            return new Gui.Widget.Static.Label;
         when Textentry =>
            return new Gui.Widget.Text.Text_Entry;
         when Picturebutton =>
            return new Gui.Widget.Button.Picture_Button;
         when Textbutton =>
            return new Gui.Widget.Button.Text_Button;
         when Checkbutton =>
            return new Gui.Widget.Check_Button.Check_Button;
         when Radiobutton =>
            return new Gui.Widget.Radio_Button.Radio_Button;
         when Picture =>
            return new Gui.Widget.Picture.Picture;
         when Listbox =>
            return new Gui.Widget.Listbox.Listbox;
         when Scale =>
            return new Gui.Widget.Scale.Scale;
         when Progress =>
            return new Gui.Widget.Progress.Progress;
         when Dropdown =>
            return new Gui.Widget.Dropdown.Dropdown;
      end case;
   end Allocate_Widget;

end Gui_Enum;