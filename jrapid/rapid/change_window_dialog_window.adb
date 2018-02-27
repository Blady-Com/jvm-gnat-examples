-- Auto generated by RAPID
-- By: Martin C. Carlisle and W. Blair Watkinson II

with Edit_Menu;
with Edit_Menu;
with Edit_Menu;


with Mcc.Gui.Fonts;

package body change_window_dialog_window is

   procedure Generate_Window is
   begin

      Mcc.Gui.Container.Window.Create(
         Obj    => change_window_dialog_window,
         X      => 0,
         Y      => 0,
         Width  => 230,
         Height => 165);

      Mcc.Gui.Container.Window.Set_Title(
         Obj   => change_window_dialog_window,
         Title => "Change Window Properties");


      Mcc.Gui.Widget.Label.Create(
         Obj     => label1,
         Parent  => change_window_dialog_window,
         X       => 0,
         Y       => 5,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Window name");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label1,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry1,
         Parent => change_window_dialog_window,
         X      => 110,
         Y      => 5,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => title_label,
         Parent  => change_window_dialog_window,
         X       => 0,
         Y       => 35,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Title");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => title_label,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry4,
         Parent => change_window_dialog_window,
         X      => 110,
         Y      => 35,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label2,
         Parent  => change_window_dialog_window,
         X       => 0,
         Y       => 65,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Width");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label2,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry2,
         Parent => change_window_dialog_window,
         X      => 110,
         Y      => 65,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label3,
         Parent  => change_window_dialog_window,
         X       => 0,
         Y       => 95,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Height");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label3,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry3,
         Parent => change_window_dialog_window,
         X      => 110,
         Y      => 95,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => applyButton,
         Parent => change_window_dialog_window,
         X      => 20,
         Y      => 125,
         Width  => 60,
         Height => 20,
         Text   => "Apply");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => applyButton'access,
         Callback => Edit_Menu.Apply_Changes'access);
      declare
         use Mcc.Gui.Fonts;
         The_Font : Mcc.Gui.Fonts.Font;
      begin
         Mcc.Gui.Fonts.Create(
            Obj    => The_Font,
            Family => SERIF,
            Size   =>  10,
            Style  => PLAIN);
         Mcc.Gui.Fonts.Set_Font(
            Obj      => Mcc.Gui.Sized_Object(applyButton),
            New_Font => The_Font);
      end;
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => okbutton,
         Parent => change_window_dialog_window,
         X      => 90,
         Y      => 125,
         Width  => 60,
         Height => 20,
         Text   => "OK");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => okbutton'access,
         Callback => Edit_Menu.Change_Done'access);
      declare
         use Mcc.Gui.Fonts;
         The_Font : Mcc.Gui.Fonts.Font;
      begin
         Mcc.Gui.Fonts.Create(
            Obj    => The_Font,
            Family => SERIF,
            Size   =>  10,
            Style  => PLAIN);
         Mcc.Gui.Fonts.Set_Font(
            Obj      => Mcc.Gui.Sized_Object(okbutton),
            New_Font => The_Font);
      end;
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => cancelButton,
         Parent => change_window_dialog_window,
         X      => 160,
         Y      => 125,
         Width  => 60,
         Height => 20,
         Text   => "Cancel");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => cancelButton'access,
         Callback => Edit_Menu.Cancel_Changes'access);
      declare
         use Mcc.Gui.Fonts;
         The_Font : Mcc.Gui.Fonts.Font;
      begin
         Mcc.Gui.Fonts.Create(
            Obj    => The_Font,
            Family => SERIF,
            Size   =>  10,
            Style  => PLAIN);
         Mcc.Gui.Fonts.Set_Font(
            Obj      => Mcc.Gui.Sized_Object(cancelButton),
            New_Font => The_Font);
      end;

   end Generate_Window;


end change_window_dialog_window;