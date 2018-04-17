with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with GNAT.Calendar; use GNAT.Calendar;
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
with GNAT.Case_Util;

package body Events is
   subtype Day_Name is GNAT.Calendar.Day_Name;
   type Week is array (Day_Name) of Boolean;

   function Extract_UID (Source : String) return String;
   function To_String (Prefix : String;
                       T       : Ada.Calendar.Time;
                       All_Day : Boolean) return String;
   function To_String (Prefix : String;
                       T       : Local_Time;
                       All_Day : Boolean) return String;

   function Get_Value (Fields : Named_Node_Map;
                       Name   : String) return String;

   procedure Set (The_Days : in out Week; Which : String);
   procedure Parse_Weekdays (The_Days : out Week; Attrs : Named_Node_Map);
   function To_String (The_Days : Week) return String;
   function To_String (The_Day : Day_Name) return String;
   function To_Day_Name (Source : String) return Day_Name;


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

   procedure Parse_Weekdays (The_Days : out Week; Attrs : Named_Node_Map) is
      function Get_Boolean (Name : String) return Boolean;

      function Get_Boolean (Name : String) return Boolean is
         S : constant String := Get_Value (Attrs, Name);
      begin
         if S = "TRUE" then
            return True;
         elsif S = "" then
            return False;
         else
            Warn ("Unknown truth value " & S & " for " & Name);
            return False;
         end if;
      end Get_Boolean;

   begin
      The_Days (Monday)    := Get_Boolean ("mo");
      The_Days (Tuesday)   := Get_Boolean ("tu");
      The_Days (Wednesday) := Get_Boolean ("we");
      The_Days (Thursday)  := Get_Boolean ("th");
      The_Days (Friday)    := Get_Boolean ("fr");
      The_Days (Saturday)  := Get_Boolean ("sa");
      The_Days (Tuesday)   := Get_Boolean ("tu");
      The_Days (Sunday)    := Get_Boolean ("su");
   end Parse_Weekdays;

   procedure Read (Data_Nodes : DOM.Core.Node_List) is

      List_Node : Node;
      All_Nodes : Node_List;

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
            The_Type        : constant String := Get_Value (Fields, "ows_EventType");
            Recurrence_ID   : constant String := Get_Value (Fields, "ows_RecurrenceID");

         begin
            if Name (Entry_Node) /= "#text" then
               if Name (Entry_Node) /= "z:row" then
                  raise Unexpected_Node with Integer'Image (I);
               end if;
               The_Event.Created := To_Time (Created_String);
               The_Event.Last_Modified := To_Time (Modified_String);
               The_Event.Event_Date := To_Time (Start_String);
               The_Event.End_Date := To_Time (End_String);
               The_Event.Recurrence_ID := To_Time (Recurrence_ID);
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
               The_Event.The_Type := To_Event_Type (The_Type);
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
               The_Event.Master_ID := To_Unbounded_String (
                                 Get_Value (Fields, "ows_MasterSeriesItemID"));
               if The_Event.Is_Recurrent then
                  The_Event.Recurrence_Data := To_Unbounded_String (
                          Unescape (Get_Value (Fields, "ows_RecurrenceData")));
               end if;
               List.Append (The_Event);
            end if;
         end;
      end loop;
   end Read;

   procedure Set (The_Days : in out Week;
                  Which    : String) is
   begin
      The_Days (To_Day_Name (Which)) := True;
   end Set;

   function To_Day_Name (Source : String) return Day_Name is
      S : String (1 .. 2) := Source;
   begin
      GNAT.Case_Util.To_Upper (S);
      if S = "MO" then
         return Monday;
      elsif S = "TU" then
         return Tuesday;
      elsif S = "WE" then
         return Wednesday;
      elsif S = "TH" then
         return Thursday;
      elsif S = "FR" then
         return Friday;
      elsif S = "SA" then
         return Saturday;
      elsif S = "SU" then
         return Sunday;
      else raise Constraint_Error with Source;
      end if;
   end To_Day_Name;

   function To_Event_Type (S : String) return Event_Type is
   begin
      if S = "0" then
         return None;
      elsif S = "1" then
         return Normal;
      elsif S = "3" then
         return Deleted;
      elsif S = "4" then
         return Excepted;
      else
         Warn ("Unknown Event_Type " & S);
         return None;
      end if;
   end To_Event_Type;

   function To_Month_Day (S : String) return Month_Day is
   begin
      return Month_Day'Value (S);
   end To_Month_Day;

   function To_String (The_Days : Week) return String is
      Result : String (1 .. 21);
      Last : Natural := 0;
   begin
      for D in The_Days'Range loop
         if The_Days (D) then
            Result (Last + 1 .. Last + 2) := To_String (D);
            Result (Last + 3) := ',';
            Last := Last + 3;
         end if;
      end loop;
      return Result (1 .. Last - 1);
   end To_String;

   function To_String (The_Day : Day_Name) return String is
      Result : String := Day_Name'Image (The_Day);
   begin
      GNAT.Case_Util.To_Upper (Result);
      return Result (1 .. 2);
   end To_String;

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

   function To_String (Prefix : String;
                       T       : Local_Time;
                       All_Day : Boolean) return String is
      The_Time : constant Ada.Calendar.Time := Ada.Calendar.Time (T);
   begin
      if All_Day then
         return Prefix & ";TZID=" & Utils.Get_Timezone & ";VALUE=DATE:"
           & GNAT.Calendar.Time_IO.Image (The_Time, "%Y%m%d");
      else
         return Prefix & ";TZID=" & Utils.Get_Timezone & ":"
           & GNAT.Calendar.Time_IO.Image (The_Time, "%Y%m%dT%H%M%S");
      end if;
   end To_String;

   function To_String (I : Interval) return String is
   begin
      return Interval'Image (I);
   end To_String;

   procedure Write (To_File : Ada.Text_IO.File_Type) is
      procedure Write_One (Position : Lists.Cursor);
      procedure Write (Text : String);
      procedure Write (Text : Unbounded_String);
      procedure Write_Recurrence (The_Event : Event);
      procedure Write_Exceptions (The_Event : Event);

      procedure Write (Text : String) is
      begin
         Ada.Text_IO.Put_Line (File => To_File,
                               Item => Text & Ada.Characters.Latin_1.CR);
      end Write;

      procedure Write (Text : Unbounded_String) is
      begin
         Write (To_String (Text));
      end Write;

      procedure Write_Exceptions (The_Event : Event) is
         procedure Count_Exceptions (Position : Lists.Cursor);
         procedure Do_Deletion (Position : Lists.Cursor);

         Sequence_Number, Counter : Natural := 0;

         procedure Count_Exceptions (Position : Lists.Cursor) is
            Excepted_Event : constant Event := Lists.Element (Position);
         begin
            if Excepted_Event.The_Type = Excepted and then
              Excepted_Event.Master_ID = The_Event.Master_ID
            then
               Counter := Counter + 1;
               --  fixme : terminate when the_event is reached
            end if;
            if Excepted_Event.Recurrence_ID = The_Event.Recurrence_ID then
               Sequence_Number := Counter;
            end if;
         end Count_Exceptions;

         procedure Do_Deletion (Position : Lists.Cursor) is
            Deleted_Event : constant Event := Lists.Element (Position);
         begin
            if Deleted_Event.The_Type = Deleted
              and then Deleted_Event.Master_ID = The_Event.Master_ID
            then
               Write (To_String ("EXDATE",
                      Deleted_Event.Recurrence_ID,
                      The_Event.Is_All_Day));
            end if;
         end Do_Deletion;

      begin
         List.Iterate (Do_Deletion'Access);
         if (The_Event.The_Type = Excepted) then
            Write (To_String ("RECURRENCE-ID",
                              The_Event.Recurrence_ID,
                              The_Event.Is_All_Day));
            List.Iterate (Count_Exceptions'Access);
         end if;

         Write ("SEQUENCE:" & To_String (Sequence_Number));
      end Write_Exceptions;

      procedure Write_One (Position : Lists.Cursor) is
         use GNAT.Calendar.Time_IO;
         Item : constant Event := Lists.Element (Position);
      begin
         if Item.The_Type = Deleted then
            return;
         end if;
         Write ("BEGIN:VEVENT");
         Write ("SUMMARY:" & Item.Summary);
         Write (To_String ("DTSTAMP", Item.Created, False));
         Write ("DESCRIPTION:" & Item.Description);
         Write ("LOCATION:" & Item.Location);
         Write ("CATEGORIES:" & Item.Category);
         Write ("UID:" & Item.UID);
         Write ("STATUS:CONFIRMED");
         Write (To_String ("LAST-MODIFIED", Item.Last_Modified, False));
         Write (To_String ("DTSTART", UTC_To_Local (Item.Event_Date),
                       Item.Is_All_Day));
         Write (To_String ("DTEND",
                UTC_To_Local (Item.Event_Date + Item.Event_Duration),
                       Item.Is_All_Day));
         Write_Recurrence (Item);
         Write_Exceptions (Item);
         Write ("END:VEVENT");
      end Write_One;

      procedure Write_Recurrence (The_Event : Event) is
         use Ada.Strings.Fixed;
         use Input_Sources.Strings;
         use GNAT.Calendar.Time_IO;

         S          : constant String := To_String (The_Event.Recurrence_Data);
         The_Days   : Week;
         Day_In_Month : Natural := 0;
         The_Month_Day : Month_Day;
         I          : Positive;
         Period     : Positive;
         XML_String : String_Input;
         The_Interval : Interval;
         Repeat_Instances : Natural := 0;
         Repeat_Forever : Boolean := False;
         Window_End : Time;
      begin
         if S = "" then
            return;
         end if;
         if S (1 .. 5) = "Every" then
            I := Index (Source => S, From => 7, Pattern => " ");
            Period := Integer'Value (S (7 .. I - 1));
            if S (I + 1 .. I + 7) = "week(s)" then
               The_Interval := weekly;
            else
               Warn ("Unsupported FREQ " & S (I + 1 .. I + 7));
            end if;
            Set (The_Days, S (I + 13 .. I + 14));
            Write ("RRULE:FREQ=" & To_String (The_Interval)
                   & ";INTERVAL=" & To_String (Period)
                   & ";BYDAY=" & To_String (The_Days)
                   & ";UNTIL=" & To_String ("",
                                            The_Event.End_Date,
                                            False) (2 .. 17));
         else
            declare
               Reader           : DOM.Readers.Tree_Reader;
               XML_Doc          : DOM.Core.Document;
               Base_Nodes, Rules,
               Rule_Items       : Node_List;

               Recurrence_Node, The_Rule,
               The_Item, Rep_Item        : Node;
               Week_Start       : String (1 .. 2);
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
               The_Rule := Item (Rules, 0);
               if Name (The_Rule) /= "rule" then
                  raise Unexpected_Node with Name (The_Rule);
               end if;

               Rule_Items := Child_Nodes (The_Rule);
               for J in 0 .. Length (Rule_Items) - 1 loop
                  The_Item := Item (Rule_Items, J);
                  if Name (The_Item) = "firstDayOfWeek" then
                     Week_Start := Value (First_Child (The_Item));
                     GNAT.Case_Util.To_Upper (Week_Start);
                  elsif Name (The_Item) = "repeat" then
                     Rep_Item := First_Child (The_Item);
                     if Name (Rep_Item) = "daily" then
                        The_Interval := daily;
                        Period := Integer'Value (
                                  Get_Value (Attributes (Rep_Item),
                                             "dayFrequency"));
                     elsif Name (Rep_Item) = "weekly" then
                        The_Interval := weekly;
                        Parse_Weekdays (The_Days, Attributes (Rep_Item));
                     elsif Name (Rep_Item) = "monthly" then
                        The_Interval := monthly;
                        Day_In_Month := Integer'Value (
                                       Get_Value (Attributes (Rep_Item),
                                                  "day"));
                        Period := Integer'Value (
                                      Get_Value (Attributes (Rep_Item),
                                                 "monthFrequency"));
                     elsif Name (Rep_Item) = "monthlyByDay" then
                        The_Interval := monthly;
                        Parse_Weekdays (The_Days, Attributes (Rep_Item));
                        Period := Integer'Value (
                                      Get_Value (Attributes (Rep_Item),
                                                 "monthFrequency"));
                        null; -- FIXME: some more elaborate variants
                        --  unsupported as yet
                     elsif Name (Rep_Item) = "yearly" then
                        The_Interval := yearly;
                        Period := Integer'Value (
                                      Get_Value (Attributes (Rep_Item),
                                                 "yearFrequency"));
                        Day_In_Month := Integer'Value (
                                      Get_Value (Attributes (Rep_Item),
                                                  "day"));
                        --  FIXME: also set month
                     elsif Name (Rep_Item) = "yearlyByDay" then
                        The_Interval := yearly;
                        Period := Integer'Value (
                                      Get_Value (Attributes (Rep_Item),
                                                 "yearFrequency"));
                        Parse_Weekdays (The_Days, Attributes (Rep_Item));
                        The_Month_Day := To_Month_Day (
                                      Get_Value (Attributes (Rep_Item),
                                                 "weekDayOfMonth"));
                     else
                        raise Unexpected_Node with Name (Rep_Item);
                     end if;
                  elsif Name (The_Item) = "repeatInstances" then
                     Repeat_Instances := Integer'Value (Value (
                                                   First_Child (The_Item)));
                  elsif Name (The_Item) = "windowEnd" then
                     Window_End := To_Time (Value (First_Child (The_Item)));
                  elsif Name (The_Item) = "repeatForever" then
                     Repeat_Forever := True;
                  elsif Name (The_Item) = "#Text" then
                     null; -- ignore
                  else
                     Warn ("Unknown tag " & Name (The_Item)
                           & "found in recurrence rule");
                  end if;
               end loop;
            end;
            Write ("RRULE:FREQ=" & To_String (The_Interval)
                   & ";INTERVAL=" & To_String (Period)
                   & ";BYDAY=" & To_String (The_Days)
                   & ";UNTIL=" & To_String ("",
                                            The_Event.End_Date,
                                            False) (2 .. 17));
         end if;
      end Write_Recurrence;

   begin
      Write ("BEGIN:VCALENDAR");
      Write ("VERSION:2.0");
      Write ("PRODID:-//aeszter@mpibpc.mpg.de//sharepoint2ics//EN");
      List.Iterate (Write_One'Access);
      Write ("END:VCALENDAR");
   end Write;

end Events;
