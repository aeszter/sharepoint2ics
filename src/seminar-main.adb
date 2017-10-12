with Ada.Command_Line;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server;
with AWS.Parameters;
with AWS.Response;
with SOAP.Client;
with SOAP.Types;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Parameters;

with Convert;

procedure Seminar.Main is
   use SOAP.Types;
   use SOAP.Parameters;
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
