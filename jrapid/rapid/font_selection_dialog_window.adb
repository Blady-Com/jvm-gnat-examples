-- Auto generated by RAPID
-- By: Martin C. Carlisle and W. Blair Watkinson II

with Font_Actions;
with Font_Actions;
with Font_Actions;


package body font_selection_dialog_window is

   procedure Generate_Window is
   begin

      Mcc.Gui.Container.Window.Create(
         Obj    => font_selection_dialog_window,
         X      => 0,
         Y      => 0,
         Width  => 257,
         Height => 171);

      Mcc.Gui.Container.Window.Set_Title(
         Obj   => font_selection_dialog_window,
         Title => "Select Font");


      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => defaultButton,
         Parent => font_selection_dialog_window,
         X      => 18,
         Y      => 127,
         Width  => 90,
         Height => 22,
         Text   => "Select Default");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => defaultButton'access,
         Callback => Font_Actions.Default_Font'access);
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => okbutton,
         Parent => font_selection_dialog_window,
         X      => 118,
         Y      => 128,
         Width  => 45,
         Height => 20,
         Text   => "OK");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => okbutton'access,
         Callback => Font_Actions.Ok_Pressed'access);
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => cancelButton,
         Parent => font_selection_dialog_window,
         X      => 174,
         Y      => 128,
         Width  => 60,
         Height => 20,
         Text   => "Cancel");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => cancelButton'access,
         Callback => Font_Actions.Cancel_Dialog'access);
      Mcc.Gui.Widget.ListBox.Create(
         Obj                  => family_box,
         Parent               => font_selection_dialog_window,
         X                    => 14,
         Y                    => 24,
         Width                => 78,
         Height               => 80,
         Horizontal_Scrollbar =>FALSE,
         Vertical_Scrollbar   =>FALSE);
      Mcc.Gui.Widget.Listbox.Set_Background_Color(
         Obj       => family_box,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.white));
      Mcc.Gui.Widget.ListBox.Create(
         Obj                  => style_box,
         Parent               => font_selection_dialog_window,
         X                    => 102,
         Y                    => 24,
         Width                => 78,
         Height               => 80,
         Horizontal_Scrollbar =>FALSE,
         Vertical_Scrollbar   =>FALSE);
      Mcc.Gui.Widget.Listbox.Set_Background_Color(
         Obj       => style_box,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.white));
      Mcc.Gui.Widget.ListBox.Create(
         Obj                  => size_box,
         Parent               => font_selection_dialog_window,
         X                    => 192,
         Y                    => 24,
         Width                => 51,
         Height               => 81,
         Horizontal_Scrollbar =>FALSE,
         Vertical_Scrollbar   =>TRUE);
      Mcc.Gui.Widget.Listbox.Set_Background_Color(
         Obj       => size_box,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.white));
      Mcc.Gui.Widget.Label.Create(
         Obj     => family_label,
         Parent  => font_selection_dialog_window,
         X       => 17,
         Y       => 7,
         Width   => 74,
         Height  => 17,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Family");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => family_label,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));
      Mcc.Gui.Widget.Label.Create(
         Obj     => style_label,
         Parent  => font_selection_dialog_window,
         X       => 103,
         Y       => 7,
         Width   => 74,
         Height  => 17,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Style");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => style_label,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));
      Mcc.Gui.Widget.Label.Create(
         Obj     => size_label,
         Parent  => font_selection_dialog_window,
         X       => 191,
         Y       => 7,
         Width   => 35,
         Height  => 17,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Size");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => size_label,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));

   end Generate_Window;


end font_selection_dialog_window;
