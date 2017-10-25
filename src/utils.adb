package body Utils is

   -----------
   -- Shift --
   -----------

   function Shift (S : String) return String is
      Result : constant String (1 .. S'Length) := S;
   begin
      return Result;
   end Shift;

end Utils;
