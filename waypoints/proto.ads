with Proto_Types;
package Proto is
    function Dist_Between_Wpts (Wpt1 : in Proto_Types.Angular;
                                Wpt2 : in Proto_Types.Angular)
                               return Proto_Types.Nautical_Miles;
end Proto;
