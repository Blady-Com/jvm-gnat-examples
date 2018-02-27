-------------------------------------------------------------------
--           RAPID - RAPID ADA PORTABLE INTERFACE DESIGNER
--           MCC GUI PACKAGE LIBRARY
--           Copyright (C) 1999 Martin C. Carlisle.
--
-- RAPID is free software; you can redistribute it and/or modify
-- it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your
-- option) any later version.  RAPID is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License
-- distributed with RAPID; see file COPYING.  If not, write to the
-- Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
-- MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License.
-- This exception does not however invalidate any other reasons
-- why the executable file might be covered by the GNU Public
-- License.  This exception does not apply to executables which
-- are GUI design tools, or that could act as a replacement
-- for RAPID.
------------------------------------------------------------------------------
with Java_Strings;
with Javax.Swing.JFrame;
with Javax.Swing.JMenu;
with Javax.Swing.JMenuItem;
with Javax.Swing.JMenuBar;
with Javax.Swing.JPanel;
with Javax.Swing.Keystroke;
with Java.Awt.Container;
with Java.Awt.Component;
with Java.Awt.Dimension;
with Java.Awt.Event.KeyEvent;
with Java.Awt.Event.InputEvent;
with Java.Awt.Point;
with Java.Lang.Object;
with Peer.Listeners;
with Mcc.Gui.Container.Window;
with Mcc.Gui.Container.Frame;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
package body Mcc.Gui.Menu is
   -- needed to set parent pointer
   type Parent_Menu is access constant Menu'Class;
   function Convert is new Ada.Unchecked_Conversion(
      Parent_Menu,Menu_Pointer);

   function Get_Keystroke(Accelerator : in String) return
      Javax.Swing.Keystroke.Ref is

      function Get_Keycode(Key : in String) return Java.Int is
         use type Java.Int;
      begin
         if Ada.Characters.Handling.To_Upper(Key(Key'First)) = 'F' and
            Key'Length > 1 then
            return Java.Int'value(Key(Key'First+1..Key'Last)) +
               Java.Awt.Event.KeyEvent.VK_F1-1;
         else
            return Java.Int(Character'Pos(Key(Key'First)));
         end if;
      end Get_Keycode;

      Modifier : Java.Int;
      Keycode  : Java.Int;
   begin
      if accelerator'length > 5 and then Ada.Characters.Handling.To_Lower(
         Accelerator(Accelerator'First..Accelerator'First+4)) = "ctrl+" then
         Modifier := Java.Awt.Event.InputEvent.CTRL_MASK;
         Keycode := Get_Keycode(
            Accelerator(Accelerator'First+5..Accelerator'Last));
      elsif accelerator'length > 4 and then Ada.Characters.Handling.To_Lower(
         Accelerator(Accelerator'First..Accelerator'First+3)) = "alt+" then
         Modifier := Java.Awt.Event.InputEvent.ALT_MASK;
         Keycode := Get_Keycode(
            Accelerator(Accelerator'First+4..Accelerator'Last));
      elsif accelerator'length > 2 and then Ada.Characters.Handling.To_Lower(
         Accelerator(Accelerator'First..Accelerator'First+2)) = "del" then
         Modifier := 0;
         Keycode := Java.Awt.Event.KeyEvent.VK_DELETE;
      elsif accelerator'length > 2 and then Ada.Characters.Handling.To_Lower(
         Accelerator(Accelerator'First..Accelerator'First+2)) = "ins" then
         Modifier := 0;
         Keycode := Java.Awt.Event.KeyEvent.VK_INSERT;
      else
         Modifier := 0;
         Keycode := Get_Keycode(Accelerator);
      end if;

      return Javax.Swing.Keystroke.Ref(Javax.Swing.KeyStroke.getKeyStroke(Keycode,Modifier,false));
   end Get_Keystroke;

   procedure Add_Choice_Common(
      Item        :    out Javax.Swing.JMenuItem.Ref;
      Obj         : in out Choice;
      To_Menu     : in     Menu'Class;
      Text        : in     String;
      Action      : in     Menu_Callback;
      Underline   : in     Natural;
      Accelerator : in     String) is
      Listener : Peer.Listeners.Menu_Action_Listener_Ref;
   begin
      Item := Javax.Swing.JMenuItem.new_JMenuItem(
         Java_Strings.To_Java_String(Text));
      Obj.My_Peer := Java.Lang.Object.Ref(Item);
      Obj.Parent  := Convert(To_Menu'Unchecked_Access);
      if Underline in Text'range then
         Javax.Swing.JMenuItem.setMnemonic(
            Item,
            Character'Pos(Text(Underline)));
      end if;

      Listener := Peer.Listeners.new_Menu_Action_Listener(
         Action);
      Javax.Swing.JMenuItem.addActionListener(
         Item,
         Listener.I_ActionListener);

      if Accelerator /= "" then
         Javax.Swing.JMenuItem.setAccelerator(
            Item,
            Get_Keystroke(Accelerator));
      end if;
   end Add_Choice_Common;
   ----------------
   -- Add_Choice --
   ----------------

   procedure Add_Choice
     (Obj         : in out Choice;
      To_Menu     : in     Menu'Class;
      Text        : in     String;
      Action      : in     Menu_Callback;
      Underline   : in     Natural;
      Accelerator : in     String := "")
   is
      Item : Javax.Swing.JMenuItem.Ref;
   begin
      Add_Choice_Common(
         Item        => Item,
         Obj         => Obj,
         To_Menu     => To_Menu,
         Text        => Text,
         Action      => Action,
         Underline   => Underline,
         Accelerator => Accelerator);

      Item := Javax.Swing.JMenuItem.Ref(Javax.Swing.JMenu.add(
         Javax.Swing.JMenu.Ref(To_Menu.My_Peer),
         P1_JMenuItem => Item));
   end Add_Choice;

   ----------------
   -- Add_Choice --
   ----------------

   procedure Add_Choice
     (Obj         : in out Choice;
      To_Menu     : in     Menu'Class;
      Text        : in     String;
      Action      : in     Menu_Callback;
      Underline   : in     Natural;
      Location    : in     Natural;
      Accelerator : in     String := "")
   is
      Item : Javax.Swing.JMenuItem.Ref;
   begin
      Add_Choice_Common(
         Item        => Item,
         Obj         => Obj,
         To_Menu     => To_Menu,
         Text        => Text,
         Action      => Action,
         Underline   => Underline,
         Accelerator => Accelerator);

      Item := Javax.Swing.JMenuItem.Ref(Javax.Swing.JMenu.Insert(
         Javax.Swing.JMenu.Ref(To_Menu.My_Peer),
         Item,
         Location));
   end Add_Choice;


   procedure Add_Submenu_Common
     (The_SubMenu : out    Javax.Swing.JMenu.Ref;
      Obj         : in out Submenu;
      Text        : in     String;
      Underline   : in     Natural;
      Parent_Menu : in     Menu'Class;
      On_Post     : in     Menu_Callback := null) is
      Listener : Peer.Listeners.Menu_Listener_Ref;
   begin
      The_Submenu := Javax.Swing.JMenu.new_JMenu(
         Java_Strings.To_Java_String(Text));
      Obj.My_Peer := Java.Lang.Object.Ref(The_SubMenu);
      Obj.Parent  := Convert(Parent_Menu'Unchecked_Access);

      if Underline in Text'range then
         Javax.Swing.JMenu.setMnemonic(
            The_Submenu,
            Character'Pos(Text(Underline)));
      end if;

      Listener := Peer.Listeners.new_Menu_Listener(
         On_Post);
      Javax.Swing.JMenu.addMenuListener(
         The_Submenu,
         Listener.I_MenuListener);
   end Add_Submenu_Common;

   -----------------
   -- Add_Submenu --
   -----------------

   procedure Add_Submenu
     (Obj         : in out Submenu;
      Text        : in     String;
      Underline   : in     Natural;
      Parent_Menu : in     Menu'Class;
      On_Post     : in     Menu_Callback := null)
   is
      The_SubMenu : Javax.Swing.JMenu.Ref;
      IgnoreJMenu : Javax.Swing.JMenuItem.Ref;
      Ignore      : Java.Awt.Component.Ref;
      Parent      : Java.Awt.Container.Ref;
   begin
      Add_Submenu_Common(
         The_SubMenu => The_SubMenu,
         Obj         => Obj,
         Text        => Text,
         Underline   => Underline,
         Parent_Menu => Parent_Menu,
         On_Post     => On_Post);

      if Parent_Menu.My_Peer.all in Javax.Swing.JMenu.Typ'Class then
         IgnoreJMenu := Javax.Swing.JMenuItem.Ref(Javax.Swing.JMenu.add(
            Javax.Swing.JMenu.Ref(Parent_Menu.My_Peer),
            P1_JMenuItem => Javax.Swing.JMenuItem.Ref(The_SubMenu)));
      elsif Parent_Menu.My_Peer.all in Javax.Swing.JMenubar.Typ'Class then
         Ignore := Java.Awt.Component.Ref(Javax.Swing.JMenubar.add(
            Javax.Swing.JMenubar.Ref(Parent_Menu.My_Peer),
            Javax.Swing.JMenuItem.Ref(The_SubMenu)));
         -- making these things visible in Java is a pain
         -- this seems to work
         Parent := Java.Awt.Container.Ref(Java.Awt.Component.getParent(
            Java.Awt.Component.Ref(Parent_Menu.My_Peer)));
         if Parent.all in Javax.Swing.JPanel.Typ'Class then
            Javax.Swing.JMenubar.setSize(
               Javax.Swing.JMenubar.Ref(Parent_Menu.My_Peer),
               Javax.Swing.JMenubar.getPreferredSize(
                  Javax.Swing.JMenubar.Ref(Parent_Menu.My_Peer)));
         end if;
         Java.Awt.Container.setVisible(
            Javax.Swing.JMenubar.getTopLevelAncestor(
               Javax.Swing.JMenubar.Ref(Parent_Menu.My_Peer)),
            true);
      else
         raise Unimplemented;
      end if;
   end Add_Submenu;

   -----------------
   -- Add_Submenu --
   -----------------

   procedure Add_Submenu
     (Obj         : in out Submenu;
      Text        : in     String;
      Underline   : in     Natural;
      Parent_Menu : in     Menu'Class;
      Location    : in     Natural;
      On_Post     : in     Menu_Callback := null)
   is
      The_SubMenu : Javax.Swing.JMenu.Ref;
      Ignore      : Javax.Swing.JMenuItem.Ref;
   begin
      if Parent_Menu in Window_Menu'Class then
         raise Unimplemented;
      end if;

      Add_Submenu_Common(
         The_SubMenu => The_SubMenu,
         Obj         => Obj,
         Text        => Text,
         Underline   => Underline,
         Parent_Menu => Parent_Menu,
         On_Post     => On_Post);

      Ignore := Javax.Swing.JMenuItem.Ref(Javax.Swing.JMenu.insert(
         Javax.Swing.JMenu.Ref(Parent_Menu.My_Peer),
         Javax.Swing.JMenuItem.Ref(The_SubMenu),
         Location));
   end Add_Submenu;

   ------------
   -- Create --
   ------------

   procedure Create
     (Obj    : in out Window_Menu;
--      Window : in     Mcc.Gui.Container.Container'Class)
      Window : in     Mcc.Gui.Object'Class)
   is
      Menubar : Javax.Swing.JMenubar.Ref;
      Ignore  : Java.Awt.Component.Ref;
      Size    : Java.Awt.Dimension.Ref;
      Frame_Location   : Java.Awt.Point.Ref;
      Content_Location : Java.Awt.Point.Ref;
   begin
      Menubar := Javax.Swing.JMenubar.new_JMenuBar;
      Obj.My_Peer := Java.Lang.Object.Ref(Menubar);

      if Window in Mcc.Gui.Container.Window.Window'Class then
         Javax.Swing.JFrame.setJMenuBar(
            Javax.Swing.JFrame.Ref(Window.My_Peer),
            Menubar);

         Size := Java.Awt.Dimension.Ref(Javax.Swing.JFrame.getSize(
            Javax.Swing.JFrame.Ref(Window.My_Peer)));

         Frame_Location := Java.Awt.Point.Ref(Javax.Swing.JFrame.getLocationOnScreen(
            Javax.Swing.JFrame.Ref(Window.My_Peer)));

         Content_Location := Java.Awt.Point.Ref(Java.Awt.Container.getLocationOnScreen(
            Javax.Swing.JFrame.getContentPane(
               Javax.Swing.JFrame.Ref(Window.My_Peer))));

         Java.Awt.Component.setSize(
            Java.Awt.Component.Ref(Window.My_Peer),
            Size.Width,
            Size.Height + (Content_Location.Y - Frame_Location.Y));
      elsif Window in Mcc.Gui.Container.Frame.Frame'Class then
         Ignore := Java.Awt.Component.Ref(Java.Awt.Container.add(
            Java.Awt.Container.Ref(Window.My_Peer),
            Java.Awt.Component.Ref(Menubar)));
      else
         raise Unimplemented;
      end if;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Obj : in out Menu_Item) is
      use type Javax.Swing.JMenu.Ref;
      Parent : Java.Awt.Container.Ref;
   begin
      -- do nothing if we are the top level menu
      if Obj.Parent = null then
         return;
      end if;

      if Obj.Parent.all in Window_Menu'Class then
         for i in 0..Javax.Swing.JMenuBar.getMenuCount(
            Javax.Swing.JMenuBar.Ref(Obj.Parent.My_Peer))-1 loop
            if Javax.Swing.JMenuBar.getMenu(
               Javax.Swing.JMenuBar.Ref(Obj.Parent.My_Peer),
               i) = Javax.Swing.JMenu.Ref(Obj.My_Peer) then
               Javax.Swing.JMenuBar.remove(
                  Javax.Swing.JMenuBar.Ref(Obj.Parent.My_Peer),
                  i);
               exit;
            end if;
         end loop;
         Parent := Java.Awt.Container.Ref(Java.Awt.Component.getParent(
            Java.Awt.Component.Ref(Obj.Parent.My_Peer)));
         if Parent.all in Javax.Swing.JPanel.Typ'Class then
            Javax.Swing.JMenubar.setSize(
               Javax.Swing.JMenubar.Ref(Obj.Parent.My_Peer),
               Javax.Swing.JMenubar.getPreferredSize(
                  Javax.Swing.JMenubar.Ref(Obj.Parent.My_Peer)));
         end if;
         Java.Awt.Container.setVisible(
            Javax.Swing.JMenubar.getTopLevelAncestor(
               Javax.Swing.JMenubar.Ref(Obj.Parent.My_Peer)),
            true);
      elsif Obj.Parent.all in Submenu'Class then
         Javax.Swing.JMenu.Remove(
            This => Javax.Swing.JMenu.Ref(Obj.Parent.My_Peer),
            P1_JMenuItem => Javax.Swing.JMenuItem.Ref(Obj.My_Peer));
      else
         raise Unimplemented;
      end if;
   end Delete;

   -------------
   -- Disable --
   -------------

   procedure Disable (Obj : in out Choice) is
   begin
      Javax.Swing.JMenuItem.setEnabled(
         Javax.Swing.JMenuItem.Ref(Obj.My_Peer),
         false);
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Obj : in out Choice) is
   begin
      Javax.Swing.JMenuItem.setEnabled(
         Javax.Swing.JMenuItem.Ref(Obj.My_Peer),
         true);
   end Enable;

   procedure Add_Separator(
      Obj         : in out Separator;
      To_Menu     : in     Menu'Class) is
   begin
      Javax.Swing.JMenu.addSeparator(
         Javax.Swing.JMenu.Ref(To_Menu.My_Peer));
   end Add_Separator;

   procedure Add_Separator(
      Obj         : in out Separator;
      To_Menu     : in     Menu'Class;
      Location    : in     Natural) is
   begin
      Javax.Swing.JMenu.insertSeparator(
         Javax.Swing.JMenu.Ref(To_Menu.My_Peer),
         Location);
   end Add_Separator;

end Mcc.Gui.Menu;

