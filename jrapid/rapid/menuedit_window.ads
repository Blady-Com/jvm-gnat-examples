-- Auto generated by RAPID
-- By: Martin C. Carlisle and W. Blair Watkinson II
with Mcc.Gui.Widget.Button.Radio;
with Mcc.Gui.Widget.Label;
with Mcc.Gui.Colors;
with Mcc.Gui.Widget.Button.Text;
with Mcc.Gui.Widget.Listbox;
with Mcc.Gui.Colors;
with Mcc.Gui.Container.Window;

package menuedit_window is
   menu_display : aliased Mcc.Gui.Widget.Listbox.Listbox;
   insert_menu : aliased Mcc.Gui.Widget.Button.Text.Text_Button;
   insert_choice : aliased Mcc.Gui.Widget.Button.Text.Text_Button;
   Insert_Separator : aliased Mcc.Gui.Widget.Button.Text.Text_Button;
   delete : aliased Mcc.Gui.Widget.Button.Text.Text_Button;
   edit : aliased Mcc.Gui.Widget.Button.Text.Text_Button;
   done : aliased Mcc.Gui.Widget.Button.Text.Text_Button;
   item_insert : aliased Mcc.Gui.Widget.Label.Label;
   inside : aliased Mcc.Gui.Widget.Button.Radio.Radio_Button;
   after : aliased Mcc.Gui.Widget.Button.Radio.Radio_Button;
   insert_group : aliased Mcc.Gui.Widget.Button.Radio.Radio_Group;

   menuedit_window : aliased Mcc.Gui.Container.Window.SubWindow;

   procedure Generate_Window;

end menuedit_window;
