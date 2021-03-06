-- Auto generated by RAPID
-- By: Martin C. Carlisle and W. Blair Watkinson II

with Subwindow_Actions;
with Subwindow_Actions;
with Subwindow_Actions;
with Subwindow_Actions;


with Mcc.Gui.Fonts;

package body label_dialog_window is

   procedure Generate_Window is
   begin

      Mcc.Gui.Container.Window.Create(
         Obj    => label_dialog_window,
         X      => 0,
         Y      => 0,
         Width  => 441,
         Height => 235);

      Mcc.Gui.Container.Window.Set_Title(
         Obj   => label_dialog_window,
         Title => "Label Properties");


      Mcc.Gui.Widget.Label.Create(
         Obj     => label1,
         Parent  => label_dialog_window,
         X       => 0,
         Y       => 35,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Widget name");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label1,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry1,
         Parent => label_dialog_window,
         X      => 110,
         Y      => 35,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label2,
         Parent  => label_dialog_window,
         X       => 0,
         Y       => 65,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "X");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label2,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry2,
         Parent => label_dialog_window,
         X      => 110,
         Y      => 65,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label3,
         Parent  => label_dialog_window,
         X       => 0,
         Y       => 95,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Y");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label3,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry3,
         Parent => label_dialog_window,
         X      => 110,
         Y      => 95,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label4,
         Parent  => label_dialog_window,
         X       => 0,
         Y       => 125,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Width");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label4,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry4,
         Parent => label_dialog_window,
         X      => 110,
         Y      => 125,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label5,
         Parent  => label_dialog_window,
         X       => 0,
         Y       => 155,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Height");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label5,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry5,
         Parent => label_dialog_window,
         X      => 110,
         Y      => 155,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label6,
         Parent  => label_dialog_window,
         X       => 220,
         Y       => 35,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Text");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label6,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry6,
         Parent => label_dialog_window,
         X      => 330,
         Y      => 35,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label7,
         Parent  => label_dialog_window,
         X       => 220,
         Y       => 65,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Justification");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label7,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry7,
         Parent => label_dialog_window,
         X      => 331,
         Y      => 65,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label8,
         Parent  => label_dialog_window,
         X       => 220,
         Y       => 95,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Foreground Color");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label8,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry8,
         Parent => label_dialog_window,
         X      => 330,
         Y      => 95,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => label9,
         Parent  => label_dialog_window,
         X       => 221,
         Y       => 125,
         Width   => 110,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Background Color");
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => label9,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Text_Entry.Create(
         Obj    => entry9,
         Parent => label_dialog_window,
         X      => 330,
         Y      => 125,
         Width  => 100,
         Height => 20);
      Mcc.Gui.Widget.Label.Create(
         Obj     => option1,
         Parent  => label_dialog_window,
         X       => 3,
         Y       => 5,
         Width   => 207,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Label Options");
      Mcc.Gui.Widget.Label.Set_Background_Color(
         Obj       => option1,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.LightBlue));
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => option1,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Label.Create(
         Obj     => option2,
         Parent  => label_dialog_window,
         X       => 220,
         Y       => 5,
         Width   => 210,
         Height  => 20,
         Justify => Mcc.Gui.Widget.Label.center,
         Text    => "Text Options");
      Mcc.Gui.Widget.Label.Set_Background_Color(
         Obj       => option2,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.LightBlue));
      Mcc.Gui.Widget.Label.Set_Foreground_Color(
         Obj       => option2,
         New_Color => Mcc.Gui.Colors.Named_Color(
            Mcc.Gui.Colors.Black));
      Mcc.Gui.Widget.Label.Create(
         Obj     => font_label,
         Parent  => label_dialog_window,
         X       => 220,
         Y       => 150,
         Width   => 110,
         Height  => 13,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Font: ");
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => change_font,
         Parent => label_dialog_window,
         X      => 330,
         Y      => 156,
         Width  => 96,
         Height => 21,
         Text   => "Change Font");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => change_font'access,
         Callback => Subwindow_Actions.Change_Font'access);
      Mcc.Gui.Widget.Label.Create(
         Obj     => font_style,
         Parent  => label_dialog_window,
         X       => 220,
         Y       => 167,
         Width   => 110,
         Height  => 15,
         Justify => Mcc.Gui.Widget.Label.left,
         Text    => "Default");
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => applyButton,
         Parent => label_dialog_window,
         X      => 130,
         Y      => 197,
         Width  => 60,
         Height => 20,
         Text   => "Apply");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => applyButton'access,
         Callback => Subwindow_Actions.Apply_Properties_Dialog'access);
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => okbutton,
         Parent => label_dialog_window,
         X      => 200,
         Y      => 197,
         Width  => 60,
         Height => 20,
         Text   => "OK");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => okbutton'access,
         Callback => Subwindow_Actions.Done_Properties_Dialog'access);
      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => cancelButton,
         Parent => label_dialog_window,
         X      => 270,
         Y      => 197,
         Width  => 60,
         Height => 20,
         Text   => "Cancel");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => cancelButton'access,
         Callback => Subwindow_Actions.Cancel_Properties_Dialog'access);

   end Generate_Window;


end label_dialog_window;
