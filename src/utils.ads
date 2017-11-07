package Utils is
   function Shift (S : String) return String;
   function Unescape (S : String) return String;
   procedure Warn (Text : String);
   function Clean_Text (Source : String) return String;
end Utils;
