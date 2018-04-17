with Ada.Text_IO;
with GNAT.Calendar;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Time_Zones;
use Ada.Calendar;


procedure Debug is
   Year, Month, Day, Hour, Minute, Second : Natural;
   Sub_Second, Seconds : Duration;
begin
   Ada.Text_IO.Put_Line (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%c"));
   GNAT.Calendar.Split (Ada.Calendar.Clock,
                        Year,
                        Month,
                        Day,
                        Hour,
                        Minute,
                        Second,
                        Sub_Second);
   Ada.Text_IO.Put_Line ("Split: " & Hour'Img & ":" & Minute'Img);
   GNAT.Calendar.Split_At_Locale (Ada.Calendar.Clock,
                                  Year,
                                  Month,
                                  Day,
                                  Hour,
                                  Minute,
                                  Second,
                                  Sub_Second);
   Ada.Text_IO.Put_Line ("Split_At_Locale: " & Hour'Img & ":" & Minute'Img);
   Ada.Calendar.Split (Ada.Calendar.Clock, Year, Month, Day, Seconds);
   Second := Integer (Seconds);
   Hour := Second / 3_600;
   Minute := (Second - Hour * 3_600) / 60;
   Ada.Text_IO.Put_Line ("Ada.Calendar.Split: " & Integer'Image (Hour) & ":" & Integer'Image (Minute));
   Ada.Text_IO.Put_Line ("TZ Offset (Clock): " & Integer'Image (Integer (Ada.Calendar.Time_Zones.UTC_Time_Offset (Ada.Calendar.Clock))));
   Ada.Text_IO.Put_Line ("TZ Offset (Clock-30d): " & Integer'Image (Integer (Ada.Calendar.Time_Zones.UTC_Time_Offset (Ada.Calendar.Clock - Duration (3_600*24*30)))));
end Debug;
