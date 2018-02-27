---------------------------------------------------------------
--                                                           
--  RAPID - Rapid Ada Portable Interface Designer
--                                                           
--  HELP_MENU.ADB
--  Description : Help Menu
--                       
--  By: Martin Carlisle 
--                                         
-- RAPID is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- RAPID is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
with Mcc.Common_Dialogs;
package body Help_Menu is

   procedure About is
   begin
      Mcc.Common_Dialogs.Ok_Box("RAPID" & Ascii.Lf &
         "Version 2.2" & Ascii.Lf &
         Ascii.Lf &
         "By: Martin C. Carlisle" & Ascii.Lf &
         "      W. Blair Watkinson II" & Ascii.Lf &
         "US Air Force Academy" & Ascii.Lf &
         "Department of Computer Science     " & Ascii.Lf &
         "carlislem@acm.org" & Ascii.Lf &
         Ascii.Lf &
         "Contributors:" & Ascii.Lf &
         "  Jonathan Busch" & Ascii.Lf &
         "  Dmitri Anisimkov" & Ascii.Lf &
         "  Pat Maes" & Ascii.Lf &
         Ascii.Lf &
         "September 21, 1999");
   end About;

end Help_Menu;
