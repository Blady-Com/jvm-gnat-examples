with Text_Io;
with Proto_Types;
with Proto;
with Calendar;  use Calendar;

procedure Main is
    Answer : Proto_Types.Nautical_Miles;
    Waypoint1 : Proto_Types.Angular := (Lat => 0.0, Lon => 0.0);
    Waypoint2 : Proto_Types.Angular := (Lat => 0.0, Lon => 0.0);
    Max : constant := 10000;
    J : Integer range 1 .. Max := 1;
    Number_Of_Printouts : constant := 10;
    Test : constant := Max / Number_Of_Printouts;
    Time1,
    Time2  : Time;
    Total : Duration;
    Millisecs : Integer;
begin
    Time1 := Calendar.Clock;
    for I in 1 .. Max loop
        Answer := Proto.Dist_Between_Wpts (Waypoint1, Waypoint2);
        Waypoint2.Lat := Waypoint2.Lat + 0.001;
        Waypoint2.Lon := Waypoint2.Lon + 0.001;
    end loop;
    Time2 := Clock;
    Text_IO.Put_Line ("Iteration finished2");
    Total := Time2 - Time1;
    Text_IO.Put_Line ("Iteration finished3");
    Millisecs := Integer (Total * 1000.0);
    Text_IO.Put_Line ("Calculated");
    Text_Io.Put_Line (Integer'Image (Max) & " iterations of waypoint in " &
                        Integer'Image (Millisecs) & " milliseconds.");
    Text_Io.Put_Line ("End of Program Waypoint");

exception
   when others =>
      Text_Io.Put_Line ("The Waypoint program died");
end Main;
