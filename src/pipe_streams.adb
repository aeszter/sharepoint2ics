with Ada.IO_Exceptions;

with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX.Process_Environment; use POSIX.Process_Environment;


package body Pipe_Streams is


   ---------------
   -- Next_Char --
   ---------------

   overriding procedure Next_Char
     (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char) is
   begin
      if From.Position >= Integer (From.Last_Read) then
         POSIX.IO.Read (File           => From.Pipe,
                        Buffer         => From.Buffer,
                        Last           => From.Last_Read);
         From.Position := 0;
      end if;
      From.Position := From.Position + 1;
      C := Unicode.To_Unicode (Standard.Character (From.Buffer (From.Position)));
   exception
      when Ada.IO_Exceptions.End_Error =>
         From.Eof_Reached := True;
         C := Unicode.To_Unicode (Standard.Character (LF));
   end Next_Char;

   ---------
   -- Eof --
   ---------

   overriding function Eof (From : Pipe_Stream) return Boolean is
   begin
      return From.Eof_Reached;
   end Eof;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Input : in out Pipe_Stream) is
      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status, Child => Input.PID);
      Input_Sources.Close (Input_Source (Input));
      case Exit_Status_Of (Status) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Failed_Creation_Error;
            when Unhandled_Exception_Exit => raise Exception_Error;
         when others => raise Other_Error with Exit_Status_Of (Status)'Img;
      end case;
   end Close;



   -------------
   -- execute --
   -------------

   procedure Execute (P : in out Pipe_Stream;
                      Command : String;
                      Arguments : POSIX_String_List)
   is
      To_QView : POSIX.IO.File_Descriptor;
      Template : Process_Template;
      Env : POSIX.Process_Environment.Environment;
   begin
      POSIX.IO.Create_Pipe (Read_End  => P.Pipe,
                            Write_End => To_QView);
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => P.Pipe);
      Set_File_Action_To_Duplicate (Template  => Template,
                                    File      => Standard_Output,
                                    From_File => To_QView);

      Start_Process (Child    => P.PID,
                     Pathname => To_POSIX_String (Command),
                     Template => Template,
                     Arg_List => Arguments,
                     Env_List => Env);
      Close (File => To_QView);
   end Execute;

end Pipe_Streams;
