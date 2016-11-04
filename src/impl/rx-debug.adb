package body Rx.Debug is

   ---------
   -- Log --
   ---------

   procedure Log (S : String; Level : Levels := Verbose) is
   begin
      if Level >= Debug.Level then
         Put_Line ("debug [" & Level'Img & "]: " & S);
      end if;
   end Log;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (I : Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;

   -----------
   -- Print --
   -----------

   procedure Print (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Put_Line ("Uh oh...");
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (Ada.Exceptions.Exception_Information (E));
   end Print;

end Rx.Debug;
