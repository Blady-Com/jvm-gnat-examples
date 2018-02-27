with Proto_Types;
package body Proto is
    Up1 : Proto_Types.Vector;
    Up2 : Proto_Types.Vector;
    Un : Proto_Types.Vector;
    function Dist_Between_Wpts (Wpt1 : in Proto_Types.Angular;
                                Wpt2 : in Proto_Types.Angular)
                               return Proto_Types.Nautical_Miles is
        --|
        --| FUNCTIONAL DESCRIPTION -
        --| This function computes the great circle distance, in nautical miles,
        --| between the two given waypoints.
        --|
        --| LIMITATIONS/NOTES -
        --| The distance between waypoints is computed based on a spherical earth
        --| model.
        --| ......................................................................
        package Mathlib renames Proto_Types.Mathlib;


        Sin_Alpha : Proto_Types.Fms_Float;
        Cos_Alpha : Proto_Types.Fms_Float;

    begin                                -- dist_between_wpts

        Up1 := Proto_Types.Unit_Pos_Vector (Wpt1.Lat, Wpt1.Lon);

        Up2 := Proto_Types.Unit_Pos_Vector (Wpt2.Lat, Wpt2.Lon);

        Un (1) := Up1 (2) * Up2 (3) - Up1 (3) * Up2 (2);
        Un (2) := Up1 (3) * Up2 (1) - Up1 (1) * Up2 (3);
        Un (3) := Up1 (1) * Up2 (2) - Up1 (2) * Up2 (1);

        Sin_Alpha := Mathlib.Sqrt (Un (1) ** 2 + Un (2) ** 2 + Un (3) ** 2);
        Cos_Alpha := Up1 (1) * Up2 (1) + Up1 (2) * Up2 (2) + Up1 (3) * Up2 (3);

        return Proto_Types.Earth_Radius * Mathlib.Arctan (Sin_Alpha, Cos_Alpha);

    end Dist_Between_Wpts;
end Proto;
