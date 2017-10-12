with Ada.Text_IO; use Ada.Text_IO;
with DOM; use DOM;
with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with DOM.Readers;
with Sax.Readers; use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;

procedure Convert (Input, Output : String) is
   XML_File : File_Input;
     ICS_File : File_Type;
      Reader : DOM.Readers.Tree_Reader;
      XML_Doc : Document;
      All_Nodes : Node_List;
      Config_Node --  , One_Node
                      : Node;

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
      Config_Node := First_Child (XML_Doc);
      All_Nodes := Child_Nodes (Config_Node);

   Create (File => ICS_File,
           Name => Output);
--   Open (File => ICS_File,
--         Mode => Out_File,
--         Name => Output);
   Close (XML_File);
   Close (ICS_File);
end Convert;

