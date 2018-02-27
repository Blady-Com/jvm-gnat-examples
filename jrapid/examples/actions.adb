with Mcc.Common_Dialogs;
package body actions is

   -------------
   -- action1 --
   -------------

   procedure action1 is
   begin
      Mcc.Common_Dialogs.OK_Box("Action1");
   end action1;
   procedure action1(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      action1;
   end action1;

   -------------
   -- action2 --
   -------------

   procedure action2 is
   begin
      Mcc.Common_Dialogs.OK_Box("Action2");
   end action2;
   procedure action2(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      action2;
   end action2;

   -------------
   -- action3 --
   -------------

   procedure action3 is
   begin
      Mcc.Common_Dialogs.OK_Box("Action3");
   end action3;
   procedure action3(Obj : in out Mcc.Gui.Widget.Button.Button'Class) is
   begin
      action3;
   end action3;

end actions;

