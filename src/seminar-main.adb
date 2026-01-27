with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with POSIX;
with POSIX.Process_Environment;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers;
with CGI;

with Convert;
with Pipe_Streams; use Pipe_Streams;
with Utils;

procedure Seminar.Main is
   use Ada.Command_Line;

   procedure Append (Source : in out POSIX.POSIX_String_List;
                     New_Item : String);
   procedure Append (Source : in out POSIX.POSIX_String_List;
                     New_Item : Unbounded_String);
   procedure Copy_Curl_Env (Name, Value : String);

   Version : constant String := "v0.4";
   Config_File         : Ada.Text_IO.File_Type;
   Curl_Command        : Pipe_Stream;
   Reader              : DOM.Readers.Tree_Reader;
   Sharepoint_Reply    : DOM.Core.Document;
   Curl_Path           : Unbounded_String;
   User_Name, Password : Unbounded_String;
   URL, Request        : Unbounded_String;
   Arguments           : POSIX.POSIX_String_List;
   Curl_Env            : POSIX.Process_Environment.Environment;
   Config_Path         : constant String := CGI.Get_Environment ("CONFIG_FILE");

   Config_Error, Parser_Error : exception;

   procedure Append (Source   : in out POSIX.POSIX_String_List;
                     New_Item : String) is
      use POSIX;
   begin
      Append (Source, To_POSIX_String (New_Item));
   end Append;

   procedure Append (Source   : in out POSIX.POSIX_String_List;
                     New_Item : Unbounded_String) is
      use POSIX;
   begin
      Append (Source, To_POSIX_String (To_String (New_Item)));
   end Append;

   procedure Copy_Curl_Env (Name, Value : String) is
      use POSIX;
      use POSIX.Process_Environment;
   begin
      if Name'Length > 5 and then
        Name (Name'First .. Name'First + 4) = "CURL_" then
         Set_Environment_Variable (Name => To_POSIX_String (Name (Name'First + 5 .. Name'Last)),
                                   Value => To_POSIX_String (Value),
                                   Env   => Curl_Env);
      end if;
   end Copy_Curl_Env;


--  AWS does not support NTLM, so we rely on curl to perform the actual SOAP call;
--
begin
   if Argument_Count = 0 and then
     Config_Path /= ""
   then
      --  read config:
      Open (Config_File, In_File, Config_Path);
      while not Ada.Text_IO.End_Of_File (Config_File) loop
         declare
            use Ada.Strings;
            use Ada.Strings.Fixed;

            Config_Line : constant String := Get_Line (Config_File);
            Separator   : constant Natural := Index (Source  => Config_Line,
                                                     Pattern => "=");
            Name        : constant String :=
                            Trim (Config_Line (1 .. Separator - 1), Right);
            Value       : constant String :=
                         Trim (Config_Line (Separator + 1 .. Config_Line'Last),
                               Left);
         begin
            if Name = "" then
               null; -- empty line
            elsif Name (1) = '#' then
               null; -- comment
            elsif Name = "curl" then
               Curl_Path := To_Unbounded_String (Value);
            elsif Name = "username" then
               User_Name := To_Unbounded_String (Value);
            elsif Name = "password" then
               Password := To_Unbounded_String (Value);
            elsif Name = "url" then
               URL := To_Unbounded_String (Value);
            elsif Name = "request" then
               Request := To_Unbounded_String (Value);
            elsif Name = "timezone" then
               Utils.Timezone := To_Unbounded_String (Value);
            else
               raise Config_Error with
                 "Unknown name """ & Name & """";
            end if;
         end;
      end loop;
      Ada.Text_IO.Close (Config_File);
--  call wget:
      Curl_Command.Set_Public_Id (To_String (Curl_Path));
      Append (Arguments, Curl_Path);
      Append (Arguments, "--anyauth");
      Append (Arguments, "--user");
      Append (Arguments, User_Name & ":" & Password);
      Append (Arguments, "--data");
      Append (Arguments, "@" & Request);
      Append (Arguments, "--header");
      Append (Arguments, "Content-Type: text/xml;charset=UTF-8");
      Append (Arguments, "--header");
      Append (Arguments, "SOAPAction: ""http://schemas.microsoft.com/sharepoint/soap/GetListItems""");
      Append (Arguments, "-o-");
      Append (Arguments, URL);

      Ada.Environment_Variables.Iterate (Copy_Curl_Env'Access);

      Curl_Command.Execute (Command => To_String (Curl_Path),
                            Arguments => Arguments,
                            Env => Curl_Env);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (Curl_Command);
      Curl_Command.Close;
      Sharepoint_Reply := Reader.Get_Tree;

      CGI.Put_CGI_Header ("Content-type: text/calendar");
      Convert (DOM.Core.Documents.Get_Elements_By_Tag_Name (
               Sharepoint_Reply, "rs:data"),
               Ada.Text_IO.Standard_Output);
      DOM.Readers.Free (Reader);
   else
      Ada.Text_IO.Put_Line ("sharepoint2ics " & Version & " config_file");
      Ada.Text_IO.Put_Line ("Usage: " & Command_Name);
      Ada.Text_IO.Put_Line ("  config is read from the file at $CONFIG_FILE ");
   end if;
   exception
      when Pipe_Streams.Failed_Creation_Error =>
         raise Parser_Error with "Failed to spawn """ & To_String (Curl_Path);
      when Pipe_Streams.Exception_Error =>
         raise Parser_Error with """" & To_String (Curl_Path)
           & """ terminated because of an unhandled exception";
      when E : Sax.Readers.XML_Fatal_Error =>
         raise Parser_Error with "Fatal XML error: " & Exception_Message (E);
      when E : others => raise Parser_Error with "Error when calling "
           & To_String (Curl_Path) & ": "
           & Exception_Message (E);

end Seminar.Main;
