with Limits;
package body Rapid_Helpers is

   procedure Rapid_Resize_Handler (
      Obj    : in out Mcc.Gui.Container.Window.Window'Class;
      Width  : in     Integer;
      Height : in     Integer) is
      use Limits;
   begin -- Rapid_Resize_Handler
      if Width < Min_Rapid_Window_Width and
         Height < Min_Rapid_Window_Height then
         Mcc.Gui.Resize(
            Obj    => Mcc.Gui.Sized_Object(Obj),
            Width  => Min_Rapid_Window_Width,
            Height => Min_Rapid_Window_Height);
      elsif Width < Min_Rapid_Window_Width then
         Mcc.Gui.Resize(
            Obj    => Mcc.Gui.Sized_Object(Obj),
            Width  => Min_Rapid_Window_Width,
            Height => Height);
      elsif Height < Min_Rapid_Window_Height then
         Mcc.Gui.Resize(
            Obj    => Mcc.Gui.Sized_Object(Obj),
            Width  => Width,
            Height => Min_Rapid_Window_Height);
      end if;
   end Rapid_Resize_Handler;
   
end Rapid_Helpers;