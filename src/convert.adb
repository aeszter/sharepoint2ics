with Ada.Text_IO; use Ada.Text_IO;
with DOM; use DOM;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers; use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;


with Events;


procedure Convert (Input, Output : String) is

   XML_File        : File_Input;
   ICS_File        : File_Type;
   Reader          : DOM.Readers.Tree_Reader;
   XML_Doc         : DOM.Core.Document;
   List_Nodes      : DOM.Core.Node_List;


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
   Events.Read (List_Nodes);

   Create (File => ICS_File,
           Name => Output);
--   Open (File => ICS_File,
--         Mode => Out_File,
--         Name => Output);
   Events.Write (ICS_File);
   Close (ICS_File);
end Convert;

