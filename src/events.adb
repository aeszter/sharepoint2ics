with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO;

with DOM.Core; use DOM.Core;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Readers;

with Input_Sources.Strings;
with Unicode.CES.Basic_8bit;

with Utils; use Utils;
with Sax;
with Sax.Readers;

package body Events is
   function Extract_UID (Source : String) return String;
   function To_String (Prefix : String;
                       T       : Ada.Calendar.Time;
                       All_Day : Boolean) return String;

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

   procedure Read (Data_Nodes : DOM.Core.Node_List) is

      function Get_Value (Fields : Named_Node_Map;
                          Name   : String) return String;
      List_Node : Node;
      All_Nodes : Node_List;

      function Get_Value (Fields : Named_Node_Map;
                          Name   : String) return String is
         The_Item : constant Node := Get_Named_Item (Fields, Name);
      begin
         if The_Item = null then
            return "";
         else
            return Value (The_Item);
         end if;
      end Get_Value;

   begin
      if Length (Data_Nodes) > 1 then
         Warn ("found more than one data node,"
                            & " using only the first one");
      end if;
      List_Node := Item (Data_Nodes, 0);
      Ada.Text_IO.Put_Line (Standard_Error, "converting "
                   & Get_Value (Attributes (List_Node), "ItemCount")
                   & " entries");
      All_Nodes := Child_Nodes (List_Node);
      for I in 0 .. Length (All_Nodes) - 1 loop
         declare
            Entry_Node      : constant Node := Item (All_Nodes, I);
            The_Event       : Event;
            Fields          : constant Named_Node_Map := Attributes (Entry_Node);
            All_Day_Event   : constant String := Get_Value (Fields, "ows_fAllDayEvent");
            Recurrent_Event : constant String := Get_Value (Fields, "ows_fRecurrence");
            Start_String    : constant String := Get_Value (Fields, "ows_EventDate");
            End_String      : constant String := Get_Value (Fields, "ows_EndDate");
            Created_String  : constant String := Get_Value (Fields, "ows_Created");
            Modified_String : constant String := Get_Value (Fields, "ows_Modified");

         begin
            if Name (Entry_Node) /= "#text" then
               if Name (Entry_Node) /= "z:row" then
                  raise Unexpected_Node with Integer'Image (I);
               end if;
               The_Event.Created := GNAT.Calendar.Time_IO.Value
                 (Shift (Created_String));
               The_Event.Last_Modified := GNAT.Calendar.Time_IO.Value
                 (Shift (Modified_String));
               The_Event.Event_Date := GNAT.Calendar.Time_IO.Value
                 (Shift (Start_String));
               The_Event.End_Date := GNAT.Calendar.Time_IO.Value
                 (Shift (End_String));
               The_Event.Event_Duration := Ada.Real_Time.To_Duration (
                                        Ada.Real_Time.Seconds (
                                        Integer'Value (
                                        Get_Value (Fields, "ows_Duration"))));
               if All_Day_Event = "0" then
                  The_Event.Is_All_Day := False;
               elsif All_Day_Event = "1" then
                  The_Event.Is_All_Day := True;
               else
                  Warn ("Unknown All_Day status """ & All_Day_Event
                        & """ found in event " & Integer'Image (I));
                  The_Event.Is_All_Day := False;
               end if;
               if Recurrent_Event = "0" then
                  The_Event.Is_Recurrent := False;
               elsif Recurrent_Event = "1" then
                  The_Event.Is_Recurrent := True;
               else
                  Warn ("Unknown Recurrence status """ & Recurrent_Event
                        & """ found in event " & Integer'Image (I));
                  The_Event.Is_Recurrent := False;
               end if;
               if The_Event.Recurrence_Data /= "" then
                  Warn (To_String (The_Event.Recurrence_Data));
               end if;
               The_Event.Summary := To_Unbounded_String (
                                    Get_Value (Fields, "ows_Title"));
               The_Event.Description := To_Unbounded_String (Clean_Text (
                                       Get_Value (Fields, "ows_Description")));
               The_Event.Location := To_Unbounded_String (
                                           Get_Value (Fields, "ows_Location"));
               The_Event.Category := To_Unbounded_String (
                                           Get_Value (Fields, "ows_Category"));
               The_Event.UID := To_Unbounded_String (
                             Extract_UID (Get_Value (Fields, "ows_UniqueId")));
               if The_Event.Is_Recurrent then
                  The_Event.Recurrence_Data := To_Unbounded_String (
                          Unescape (Get_Value (Fields, "ows_RecurrenceData")));
               end if;
            end if;
            List.Append (The_Event);
         end;
      end loop;
   end Read;

   function To_String (Prefix : String;
                       T       : Ada.Calendar.Time;
                       All_Day : Boolean) return String is
   begin
      if All_Day then
         return Prefix & ";VALUE=DATE:"
           & GNAT.Calendar.Time_IO.Image (T, "%Y%m%d");
      else
         return Prefix & ":"
           & GNAT.Calendar.Time_IO.Image (T, "%Y%m%dT%H%M%SZ");
      end if;
   end To_String;

   procedure Write (To_File : Ada.Text_IO.File_Type) is
      procedure Write_One (Position : Lists.Cursor);
      procedure Write (Text : String);
      procedure Write (Text : Unbounded_String);
      procedure Write_Recurrence (The_Event : Event);

      procedure Write (Text : String) is
      begin
         Ada.Text_IO.Put_Line (File => To_File,
                               Item => Text & Ada.Characters.Latin_1.CR);
      end Write;

      procedure Write (Text : Unbounded_String) is
      begin
         Write (To_String (Text));
      end Write;

      procedure Write_One (Position : Lists.Cursor) is
         use GNAT.Calendar.Time_IO;
         Item : constant Event := Lists.Element (Position);
      begin
         Write ("BEGIN:VEVENT");
         Write ("SUMMARY:" & Item.Summary);
         Write (To_String ("DTSTAMP", Item.Created, False));
         Write ("DESCRIPTION:" & Item.Description);
         Write ("LOCATION:" & Item.Location);
         Write ("CATEGORIES:" & Item.Category);
         Write ("UID:" & Item.UID);
         Write ("STATUS:CONFIRMED");
         Write (To_String ("LAST-MODIFIED", Item.Last_Modified, False));
         Write (To_String ("DTSTART", Item.Event_Date, Item.Is_All_Day));
         Write (To_String ("DTEND", Item.Event_Date + Item.Event_Duration,
                       Item.Is_All_Day));
         Write_Recurrence (Item);
         Write ("SEQUENCE:0");
         Write ("END:VEVENT");
      end Write_One;

      procedure Write_Recurrence (The_Event : Event) is
         use Ada.Strings.Fixed;
         use Input_Sources.Strings;
         use GNAT.Calendar.Time_IO;

         S          : constant String := To_String (The_Event.Recurrence_Data);
         Result     : String (1 .. 1_024);
         Last       : Natural := 0;
         I, Len     : Positive;
         Period     : Positive;
         XML_String : String_Input;
      begin
         if S = "" then
            return;
         end if;
         Result (1 .. 6) := "RRULE:";
         Last := 6;
         if S (1 .. 5) = "Every" then
            Result (Last + 1 .. Last + 5) := "FREQ:";
            Last := Last + 5;
            I := Index (Source => S, From => 7, Pattern => " ");
            Period := Integer'Value (S (7 .. I - 1));
            if S (I + 1 .. I + 7) = "week(s)" then
               Result (Last + 1 .. Last + 16) := "WEEKLY;INTERVAL=";
               Last := Last + 16;
            else
               Warn ("Unsupported FREQ " & S (I + 1 .. I + 7));
            end if;
            Len := Integer'Image (Period)'Length - 1; -- leading space
            Result (Last + 1 .. Last + Len) :=
              Integer'Image (Period) (2 .. Len + 1);
            Last := Last + Len;
            Result (Last + 1 .. Last + 8) := ";WKST=MO";
            Last := Last + 8;
            Result (Last + 1 .. Last + 7) := ";BYDAY=";
            Last := Last + 7;
            Result (Last + 1 .. Last + 2) := S (I + 13 .. I + 14);
            Last := Last + 2;
            Result (Last + 1 .. Last + 7) := ";UNTIL=";
            Last := Last + 7;
            Result (Last + 1 .. Last + 16) := To_String ("",
                                                         The_Event.End_Date,
                                                         False) (2 .. 17);
            Last := Last + 16;
         else
            declare
               Reader          : DOM.Readers.Tree_Reader;
               XML_Doc         : DOM.Core.Document;
               Base_Nodes, Rules, Rule_Items       : Node_List;
               Recurrence_Node, The_Rule, The_Item : Node;
            begin
               Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
               Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
               Open (Str      => To_String (The_Event.Recurrence_Data),
                     Encoding => Unicode.CES.Basic_8bit.Basic_8bit_Encoding,
                     Input    => XML_String);
               Reader.Parse (XML_String);
               Close (XML_String);
               XML_Doc := Reader.Get_Tree;
               Base_Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name (
                               XML_Doc, "recurrence");
               if (Length (Base_Nodes) > 1) then
                  Warn ("Found more than 1 recurrence node in item, "
                     & "using only the first one");
                  Warn (S);
               end if;
               Recurrence_Node := Item (Base_Nodes, 0);
               Rules := Child_Nodes (Recurrence_Node);
               for I in 0 .. Length (Rules) - 1 loop
                  The_Rule := Item (Rules, I);
                  if Name (The_Rule) = "rule" then
                     Rule_Items := Child_Nodes (The_Rule);
                     for J in 0 .. Length (Rule_Items) - 1 loop
                        The_Item := Item (Rule_Items, J);
                        if Name (The_Item) = "firstDayOfWeek" then
                           Result (Last + 1 .. Last + 6) := ";WKST=";
                           Last := Last + 6;
                           Result (Last + 1 .. Last + 2)
                             := Value (First_Child (The_Item));
                           Last := Last + 2;
                        elsif Name (The_Item) = "repeat" then
                           null; -- FIXME
                        elsif Name (The_Item) = "repeatInstances" then
                           null; -- FIXME
                        elsif Name (The_Item) = "windowEnd" then
                           null; -- FIXME
                        elsif Name (The_Item) = "repeatForever" then
                           null; -- FIXME
                        elsif Name (The_Item) = "#Text" then
                           null; -- ignore
                        else
                           Warn ("Unknown tag " & Name (The_Item)
                                 & "found in recurrence rule");
                        end if;
                     end loop;
                  end if;
               end loop;
            end;
         end if;
         Write (Result (1 .. Last));
      end Write_Recurrence;

   begin
      Write ("BEGIN:VCALENDAR");
      Write ("VERSION:2.0");
      Write ("PRODID:-//aeszter@mpibpc.mpg.de//sharepoint2ics//EN");
      List.Iterate (Write_One'Access);
      Write ("END:VCALENDAR");
   end Write;

end Events;
