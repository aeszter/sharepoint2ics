with Ada.Calendar;
with POSIX;

package Utils is
   function Shift (S : String) return String;
   function Unescape (S : String) return String;
   procedure Warn (Text : String);
   function Clean_Text (Source : String) return String;
   function To_Time (Source : String) return Ada.Calendar.Time;
   function To_String (N : Natural) return String;
   procedure To_String_List
     (Source  : String;
      Dest   : out POSIX.POSIX_String_List);
end Utils;
