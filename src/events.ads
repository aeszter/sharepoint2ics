with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings;
with Ada.Strings.Unbounded;

with DOM.Core;

package Events is
   type Event is record
      Created, Last_Modified,
      Event_Date, End_Date     : Ada.Calendar.Time;
      Event_Duration           : Duration;
      Is_All_Day, Is_Recurrent : Boolean;
      Summary, Description     : Ada.Strings.Unbounded.Unbounded_String;
      Location, Category       : Ada.Strings.Unbounded.Unbounded_String;
      UID, Recurrence_Data     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Unexpected_Node : exception;

   package Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Event);
   List : Lists.List;
   procedure Read (Data_Nodes : DOM.Core.Node_List);
   procedure Write (To_File : Ada.Text_IO.File_Type);
end Events;
