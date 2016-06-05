with Gnat.Traceback.Symbolic;

package body Rx.Debug is

   procedure Log (S : String) is
   begin
      if Enabled then
         Put_Line ("debug: " & S);
      end if;
   end Log;

   -----------
   -- Print --
   -----------

   procedure Print (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Put_Line ("Uh oh...");
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (Ada.Exceptions.Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Print;

   procedure Put_Line (I : Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;

end Rx.Debug;
