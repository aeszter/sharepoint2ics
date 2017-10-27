package body Utils is

   -----------
   -- Shift --
   -----------

   function Shift (S : String) return String is
      Result : constant String (1 .. S'Length) := S;
   begin
      return Result;
   end Shift;

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


end Utils;
