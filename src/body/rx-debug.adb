package body Rx.Debug is

   ----------
   -- Head --
   ----------

   function Head (S : String; Sep : Character := ' ') return String is
   begin
      for I in S'Range loop
         if S (I) = Sep then
            return S (S'First .. I - 1);
         end if;
      end loop;

      return S;
   end Head;

   -----------
   -- Trace --
   -----------

   procedure Trace (S : String; Prefix : String := GNAT.Source_Info.Source_Location) is
   begin
      pragma Warnings (Off);
      if Level = Impl then -- To allow dead code removal when inlining
         Log (S & " @ " & Head (Prefix), Impl);
      end if;
      pragma Warnings (On);
   end Trace;

   ---------
   -- Log --
   ---------

   procedure Log (S : String; Level : Levels) is
   begin
      pragma Warnings (Off);
      if Level >= Debug.Level then
         Put_Line ("debug [" & Level'Img & "]: " & S);
      end if;
      pragma Warnings (On);
   end Log;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (I : Rx_Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;

   -----------
   -- Print --
   -----------

   procedure Print (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (Ada.Exceptions.Exception_Information (E));
   end Print;

   ------------
   -- Report --
   ------------

   procedure Report (E       : Ada.Exceptions.Exception_Occurrence;
                     Msg     : String;
                     Level   : Levels := Error;
                     Reraise : Boolean := False)
   is
   begin
      Log (Msg, Level);
      Print (E);
      if Reraise then
         Ada.Exceptions.Reraise_Occurrence (E);
      end if;
   end Report;

end Rx.Debug;
