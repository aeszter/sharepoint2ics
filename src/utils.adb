with Ada.Text_IO;
with Ada.Characters;
with Ada.Characters.Latin_1;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;

package body Utils is

   function Clean_Text (Source : String) return String is
      use Ada.Characters.Latin_1;
      Result : String (1 .. 2 * Source'Length);
      K : Natural := 0;
   begin
      for I in Source'Range loop
         if Source (I) = LF then
            K := K + 1;
            Result (K) := '\';
            K := K + 1;
            Result (K) := 'n';
         elsif Source (I) = CR then
            null; -- skip
         else
            K := K + 1;
            Result (K) := Source (I);
         end if;
      end loop;
      return Result (1 .. K);
   end Clean_Text;

   function Shift (S : String) return String is
      Result : constant String (1 .. S'Length) := S;
   begin
      return Result;
   end Shift;

   function To_String (N : Natural) return String is
      S : constant String := Integer'Image (N);
   begin
      return S (2 .. S'Last);
   end To_String;

   function To_Time (Source : String) return Ada.Calendar.Time is
      use GNAT.Calendar;
   begin
      return Time_IO.Value (Shift (Source));
   exception
      when Constraint_Error =>
         return Time_Of (Year    => 1999,
                         Month   => 12,
                         Day     => 31,
                         Hour    => 23,
                         Minute  => 59,
                         Second  => 53);
   end To_Time;

   function Unescape (S : String) return String is
      Result : String (1 .. S'Length);
      I      : Positive := S'First;
      R      : Positive := 1;
      J      : Positive;
   begin
      while I <= S'Last loop
         if S (I) = '&' then
            J := I + 1;
            while S (J) /= ';' loop
               J := J + 1;
            end loop;
            if S (I + 1 .. J - 1) = "gt" then
               Result (R) := '>';
            elsif S (I + 1 .. J - 1) = "lt" then
               Result (R) := '<';
            else
               Result (R .. R + 2) := "###";
               R := R + 2;
            end if;
            R := R + 1;
            I := J + 1;
         else
            Result (R) := S (I);
            R := R + 1;
            I := I + 1;
         end if;
      end loop;
      return Result (1 .. R - 1);
   end Unescape;

   procedure Warn (Text : String) is
      use Ada.Text_IO;
   begin
      Put_Line (File => Standard_Error,
                Item => "Warning: " & Text);
   end Warn;

end Utils;
