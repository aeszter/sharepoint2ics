with Ada.Command_Line;
with Ada.Text_IO;

with Convert;

procedure Seminar.Main is
   use Ada.Command_Line;

--  AWS does not support NTLM, so we rely on wget to perform the actual SOAP call;
--
begin
   if Argument_Count = 2 then
      Convert (Input => Argument (1),
              Output => Argument (2));
   else
      Ada.Text_IO.Put_Line ("Usage: calendar infile.xml outfile.ics");
   end if;

end Seminar.Main;
