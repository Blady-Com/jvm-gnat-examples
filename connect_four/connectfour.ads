--------------------------------------------------------------
-- Connect Four (TM) GNAPPLET
--
-- By:  Barry Fagin and Martin Carlisle
-- US Air Force Academy, Department of Computer Science
-- mailto:carlislem@acm.org
--
-- Adapted for JVM-GNAT GPL 2009 by Pascal Pignard
-- http://blady.pagesperso-orange.fr
--
-- This is free software; you can redistribute it and/or
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--------------------------------------------------------------

with Java.Awt.Event.Mouselistener;
with Java.Awt.Event.MouseEvent;
with Java.Awt.Image;
with Java.Awt.Image.ImageObserver;
with Java.Awt.Graphics;
with Java.Applet.Applet;
with Java.Lang.String;
with Java.IO.Serializable;
with Java.Awt.MenuContainer;
with Javax.Accessibility.Accessible;

package ConnectFour is

   --  Adding these discriminants to a type is the magic way of
   --  telling JVM-GNAT that you are implementing these interfaces.
   type Typ
     (I_Serializable  : java.io.Serializable.Ref;
      I_MenuContainer : java.awt.MenuContainer.Ref;
      I_ImageObserver : java.awt.image.ImageObserver.Ref;
      I_MouseListener : Java.Awt.Event.Mouselistener.Ref;
      I_Accessible    : Javax.Accessibility.Accessible.Ref)
   is new Java.Applet.Applet.Typ (I_MenuContainer,
                                  I_ImageObserver,
                                  I_Serializable,
                                  I_Accessible)
   with record
      User_Turn : Boolean;
   end record;
   type Ref is access all Typ'Class;
   pragma Convention (Java, Typ);

   --  The following are the specifications for the overridden
   --  methods from the Applet and MouseListener interfaces

   function GetAppletInfo (This : access Typ) return Java.Lang.String.Ref;

   procedure Init (This : access Typ);

   procedure Paint (This : access Typ;
                    G1   : access Java.Awt.Graphics.Typ'Class);

   procedure Update (This : access Typ;
                     G    : access java.awt.Graphics.Typ'Class);

   procedure mouseReleased (This : access Typ;
                            E : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseReleased);

   procedure mouseClicked (This : access Typ;
                           P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseClicked);

   procedure mouseEntered (This : access Typ;
                           P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseEntered);

   procedure mouseExited (This : access Typ;
                          P1 : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MouseExited);

   procedure mousePressed (This : access Typ;
                           E : access java.awt.event.MouseEvent.Typ'Class);
   pragma Convention (Java, MousePressed);

end ConnectFour;
