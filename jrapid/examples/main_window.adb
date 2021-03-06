-- Auto generated by RAPID
-- By: Martin C. Carlisle and W. Blair Watkinson II

with Actions;
with Actions;

with Actions;
with Actions;

package body main_window is

   procedure Generate_Window is
   begin

      Mcc.Gui.Container.Window.Create(
         Obj    => main_window,
         X      => 0,
         Y      => 0,
         Width  => 300,
         Height => 300);

      Mcc.Gui.Container.Window.Set_Title(
         Obj   => main_window,
         Title => "main");

      Mcc.Gui.Menu.Create(
         Obj    => main_Menu,
         Window => main_window);

      Mcc.Gui.Menu.Add_SubMenu(
         Obj         => Actions_SubMenu,
         Text        => "Actions",
         Underline   => 0,
         Parent_Menu => main_Menu);
      Mcc.Gui.Menu.Add_Choice(
         Obj         => Action1_Choice,
         To_Menu     => Actions_SubMenu,
         Text        => "Action1",
         Action      => Actions.Action1'access,
         Underline   => 6,
         Accelerator => "Ctrl+1");
      Mcc.Gui.Menu.Add_Choice(
         Obj         => Action2_Choice,
         To_Menu     => Actions_SubMenu,
         Text        => "Action2",
         Action      => Actions.Action2'access,
         Underline   => 6,
         Accelerator => "Ctrl+2");

      Mcc.Gui.Widget.Button.Text.Create(
         Obj    => action1,
         Parent => main_window,
         X      => 38,
         Y      => 44,
         Width  => 95,
         Height => 21,
         Text   => "Action number 1");
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => action1'access,
         Callback => Actions.action1'access);
      Mcc.Gui.Image.Create(
         Obj  => cut_gif_gif_Image,
         Name => "cut_gif.gif");
      Mcc.Gui.Widget.Button.Picture.Create(
         Obj    => cut,
         Parent => main_window,
         X      => 50,
         Y      => 89,
         Width  => 53,
         Height => 20,
         Image  => cut_gif_gif_Image);
      Mcc.Gui.Widget.Button.Set_Push_Callback(
         Obj      => cut'access,
         Callback => Actions.Action2'access);

   end Generate_Window;


end main_window;
