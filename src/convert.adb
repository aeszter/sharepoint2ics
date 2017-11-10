with Ada.Text_IO; use Ada.Text_IO;
with DOM; use DOM;

with Events;

procedure Convert (Input : DOM.Core.Node_List; Output : File_Type) is
begin
   Events.Read (Input);
   Events.Write (Output);
end Convert;

