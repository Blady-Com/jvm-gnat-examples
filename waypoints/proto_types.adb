package body Proto_Types is
    Maximum_Pole : Radians := 89.99999 * Rad_Per_Deg;
    Minimum_Pole : Radians := -Maximum_Pole;
    Vec : Three_Vector;          --temporary vector quantity
    function Unit_Pos_Vector
                (Lat : in Fms_Float; Lon : in Fms_Float) return Three_Vector is
        --|
        --| FUNCTIONAL DESCRIPTION -
        --|
        --|     This function returns the unit position vector
        --|     at the given lat and lon position.
        --|
        --|
        --| LIMITATIONS/NOTES -
        --|     The unit position vector is the unit vertical
        --|     vector that results from the normal ECEF to
        --|     local level transformation, ie Up is:
        --|
        --|           | clat * clon |
        --|           | clat * slon |
        --|           |   slat      |
        --|
        --|......................................................................

        Clat : Fms_Float;
        Tmp_Lat : Fms_Float := Lat;

    begin --unit_pos_vector
        if Lat > Maximum_Pole then
            Tmp_Lat := Maximum_Pole;
        elsif Lat < Minimum_Pole then
            Tmp_Lat := Minimum_Pole;
        end if;

        Clat := Mathlib.Cos (Tmp_Lat);      --cosine of latitude

        Vec (1) := Clat * Mathlib.Cos (Lon);
        Vec (2) := Clat * Mathlib.Sin (Lon);
        Vec (3) := Mathlib.Sin (Tmp_Lat);

        return Vec;

    end Unit_Pos_Vector;

end Proto_Types;

