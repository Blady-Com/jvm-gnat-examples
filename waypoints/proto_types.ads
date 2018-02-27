with Ada.Numerics.Generic_Elementary_Functions;
package Proto_Types is

    Pi : constant := 3.1415_92653_58979_32384_623643;

    Rad_Per_Deg : constant := (Pi / 180.0);

    Earth_Radius : constant := 3438.0;

    subtype Fms_Float is Float digits 6;
    -- gives a 21 bit => 23 bit mantissa.

    package Mathlib is
       new Ada.Numerics.Generic_Elementary_Functions (Fms_Float);

    subtype Nautical_Miles is Fms_Float;

    subtype Radians is Fms_Float;

    type Angular is
        record
            Lat : Radians;
            Lon : Radians;
        end record;

    type Vector is array (1 .. 3) of Fms_Float;

    subtype Three_Vector is Vector;

    function Unit_Pos_Vector
                (Lat : in Fms_Float; Lon : in Fms_Float) return Three_Vector;
end Proto_Types;
