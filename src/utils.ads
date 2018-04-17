with Ada.Calendar;
with Ada.Strings.Unbounded;

package Utils is
   type Local_Time is new Ada.Calendar.Time;
   Timezone : Ada.Strings.Unbounded.Unbounded_String;

   function Shift (S : String) return String;
   function Unescape (S : String) return String;
   procedure Warn (Text : String);
   function Clean_Text (Source : String) return String;
   function To_Time (Source : String) return Ada.Calendar.Time;
   function To_String (N : Natural) return String;
   function UTC_To_Local (T : Ada.Calendar.Time) return Local_Time;
   function Get_Timezone return String;
end Utils;
