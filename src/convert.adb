with Ada.Text_IO; use Ada.Text_IO;
with DOM; use DOM;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with DOM.Readers;
with Sax.Readers; use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with Ada.Calendar;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;


procedure Convert (Input, Output : String) is
   function Image (Date    : Ada.Calendar.Time;
                   Picture : GNAT.Calendar.Time_IO.Picture_String)
                   return String
                   renames GNAT.Calendar.Time_IO.Image;
   function Clean_Text (Source : String) return String;
   function Get_Value (Fields : Named_Node_Map; Name : String) return String;
   function Extract_UID (Source : String) return String;
   procedure Warn (Text : String);
   procedure Write (Text : String);

   XML_File : File_Input;
     ICS_File : File_Type;
      Reader : DOM.Readers.Tree_Reader;
      XML_Doc : Document;
      All_Nodes, List_Nodes : Node_List;
   List_Node             : Node;

   Unexpected_Node       : exception;

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

   function Extract_UID (Source : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      First, Last : Natural;
   begin
      First := Index (Source  => Source,
                      Pattern => "{",
                      Going   => Forward);
      Last := Index (Source  => Source,
                     Pattern => "}",
                     Going   => Backward);
      if First >= Source'First and then Last > First + 1 then
         return Source (First + 1 .. Last - 1);
      else -- not found in example data, may need to refine this later
         return Source;
      end if;
   end Extract_UID;

   function Get_Value (Fields : Named_Node_Map; Name : String) return String is
      The_Item : constant Node := Get_Named_Item (Fields, Name);
   begin
      if The_Item = null then
         return "";
      else
         return Value (The_Item);
      end if;
   end Get_Value;

   procedure Warn (Text : String) is
   begin
      Ada.Text_IO.Put_Line (File => Standard_Error,
                            Item => "Warning: " & Text);
   end Warn;

   procedure Write (Text : String) is
   begin
      Ada.Text_IO.Put_Line (File => ICS_File,
                            Item => Text & Ada.Characters.Latin_1.CR);
   end Write;

begin
--     Open (File    => XML_File,
--           Mode    => In_File,
--           Name    => Input
--          );
   Open (Filename => Input,
         Input => XML_File);
   Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
   Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
   Reader.Parse (XML_File);
   Close (Input => XML_File);
   XML_Doc := Reader.Get_Tree;
   List_Nodes := Get_Elements_By_Tag_Name (XML_Doc, "rs:data");
   Create (File => ICS_File,
           Name => Output);
--   Open (File => ICS_File,
--         Mode => Out_File,
--         Name => Output);

   if Length (List_Nodes) > 1 then
      Ada.Text_IO.Put_Line (Standard_Error, "found more than one data node,"
                            & " using only the first one");
   end if;
   List_Node := Item (List_Nodes, 0);
   Ada.Text_IO.Put_Line (Standard_Error, "converting "
                & Get_Value (Attributes (List_Node), "ItemCount")
                & " entries");
   All_Nodes := Child_Nodes (List_Node);
   Write ("BEGIN:VCALENDAR");
   Write ("VERSION:2.0");
   Write ("PRODID:-//aeszter@mpibpc.mpg.de//sharepoint2ics//EN");
   for I in 0 .. Length (All_Nodes) - 1 loop
      declare
         Entry_Node           : constant Node := Item (All_Nodes, I);
         Fields               : constant Named_Node_Map := Attributes (Entry_Node);
         Created, Last_Modified,
         Event_Date, End_Date : Ada.Calendar.Time;
         Is_All_Day           : Boolean;
         All_Day_Event        : constant String := Get_Value (Fields, "ows_fAllDayEvent");
      begin
         if Name (Entry_Node) /= "#text" then
            if Name (Entry_Node) /= "z:row" then
               raise Unexpected_Node with Integer'Image (I);
            end if;
            Created := GNAT.Calendar.Time_IO.Value
              (Get_Value (Fields, "ows_Created"));
            Last_Modified := GNAT.Calendar.Time_IO.Value
              (Get_Value (Fields, "ows_Modified"));
            Event_Date := GNAT.Calendar.Time_IO.Value
              (Get_Value (Fields, "ows_EventDate"));
            End_Date := GNAT.Calendar.Time_IO.Value
              (Get_Value (Fields, "ows_EndDate"));
            if All_Day_Event = "0" then
               Is_All_Day := False;
            elsif All_Day_Event = "1" then
               Is_All_Day := True;
            else
               Warn ("Unknown All_Day status """ & All_Day_Event
                     & """ found in event " & Integer'Image (I));
               Is_All_Day := False;
            end if;
            Write ("BEGIN:VEVENT");
            Write ("SUMMARY:"
                    & Get_Value (Fields, "ows_Title"));
            Write ("DTSTAMP:" & Image (Created, "%Y%m%dT%H%M%SZ"));
            Write ("DESCRIPTION:"
                   & Clean_Text (Get_Value (Fields, "ows_Description")));
            Write ("LOCATION:" & Get_Value (Fields, "ows_Location"));
            Write ("CATEGORIES:" & Get_Value (Fields, "ows_Category"));
            Write ("UID:" & Extract_UID (Get_Value (Fields, "ows_UniqueId")));
            Write ("STATUS:CONFIRMED");
            Write ("LAST-MODIFIED:"
                   & Image (Last_Modified, "%Y%m%dT%H%M%SZ"));
            if Is_All_Day then
               Write ("DTSTART;VALUE=DATE:" & Image (Event_Date, "%Y%m%d"));
               Write ("DTEND;VALUE=DATE:" & Image (End_Date, "%Y%m%d"));
            else
               Write ("DTSTART:" & Image (Event_Date, "%Y%m%dT%H%M%SZ"));
               Write ("DTEND:" & Image (End_Date, "%Y%m%dT%H%M%SZ"));
            end if;
            Write ("SEQUENCE:0");
            Write ("END:VEVENT");
         end if;
      end;
   end loop;
   Write ("END:VCALENDAR");

   Close (XML_File);
   Close (ICS_File);
end Convert;

