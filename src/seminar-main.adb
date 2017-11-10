with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers;

with Convert;
with Pipe_Streams; use Pipe_Streams;

procedure Seminar.Main is
   use Ada.Command_Line;

   Version : constant String := "develop";
   Config_File : Ada.Text_IO.File_Type;
   Wget_Command : Pipe_Stream;
   Reader       : DOM.Readers.Tree_Reader;
   Sharepoint_Reply : DOM.Core.Document;
   Wget_Path        : Unbounded_String;

   Config_Error, Parser_Error : exception;

--  AWS does not support NTLM, so we rely on wget to perform the actual SOAP call;
--
begin
   if Argument_Count = 1 then
      --  read config:
      Open (Config_File, In_File, Argument (1));
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
            if Name = "wget" then
               Wget_Path := To_Unbounded_String (Value);
            elsif Name (1) = '#' then
               null; -- comment
            elsif Name = "" then
               null; -- empty line
            else
               raise Config_Error with
                 "Unknown name """ & Name & """";
            end if;
         end;
      end loop;
      Ada.Text_IO.Close (Config_File);
--  call wget:
      Wget_Command.Set_Public_Id (To_String (Wget_Path));
      Wget_Command.Execute (Command => To_String (Wget_Path),
                           Arguments => "",
                           Environment => "");
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (Wget_Command);
      Wget_Command.Close;
      Sharepoint_Reply := Reader.Get_Tree;

      Convert (DOM.Core.Documents.Get_Elements_By_Tag_Name (
               Sharepoint_Reply, "rs:data"),
               Ada.Text_IO.Standard_Output);
      DOM.Readers.Free (Reader);
   else
      Ada.Text_IO.Put_Line ("sharepoint2ics " & Version & " config_file");
      Ada.Text_IO.Put_Line ("Usage: " & Command_Name);
   end if;
   exception
      when Pipe_Streams.Failed_Creation_Error =>
         raise Parser_Error with "Failed to spawn """ & To_String (Wget_Path)
           & """ with """ & "" & """";
      when Pipe_Streams.Exception_Error =>
         raise Parser_Error with """" & To_String (Wget_Path)
           & """ terminated because of an unhandled exception";
      when E : Sax.Readers.XML_Fatal_Error =>
         raise Parser_Error with "Fatal XML error: " & Exception_Message (E);
      when E : others => raise Parser_Error with "Error when calling "
           & To_String (Wget_Path) & " with " & """""" & ": "
           & Exception_Message (E);

end Seminar.Main;
