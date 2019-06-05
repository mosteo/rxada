with Ada.Command_Line;

with GNAT.OS_Lib;

package body Rx.Debug is

   -------------
   -- Bailout --
   -------------

   procedure Bailout (Exit_Code : Integer := 0) is
   begin
      GNAT.OS_Lib.OS_Exit (Exit_Code);
   end Bailout;

   ------------
   -- Tracer --
   ------------

   protected Tracer is
      procedure Put_Line (S : String);
   end Tracer;

   protected body Tracer is
      procedure Put_Line (S : String) is
      begin
         Gnat.IO.Put_Line (S);
      end Put_Line;
   end Tracer;


   type Tracing_States is (Off, Unknown, On);

   Tracing : Tracing_States := Unknown with Atomic;

   -------------------
   -- Check_Tracing --
   -------------------

   procedure Check_Tracing is
      use Ada.Command_Line;
   begin
      Tracing := Off;
      for I in 1 .. Argument_Count loop
         if Argument (I) = "-vvv" then
            Tracing := On;
         end if;
      end loop;
   end Check_Tracing;

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
      if Tracing = Unknown then
         Check_Tracing;
      end if;

      --  Trace when either log level demands it, or command-line -vvv given

      pragma Warnings (Off);
      if Level > Impl and then Tracing = On then
         declare
            Line : constant String := "trace: " & S & " @ " & Head (Prefix);
         begin
            if Serialize_Trace then
               Tracer.Put_Line (Line);
            else
               Put_Line (Line);
            end if;
         end;
      end if;

      if Level = Impl then
         Log (S & " @ " & Head (Prefix), Impl);
      end if;
      pragma Warnings (On);
   end Trace;

   procedure Trace (E       : Ada.Exceptions.Exception_Occurrence;
                    Msg     : String) is
   begin
      Trace ("---8<---Exception dump---8<---");
      Trace (Msg);
      Trace (Ada.Exceptions.Exception_Name (E));
      Trace (Ada.Exceptions.Exception_Message (E));
      Trace (Ada.Exceptions.Exception_Information (E));
      Trace ("---8<---Exception end----8<---");
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
      Put_Line ("---8<---Exception dump---8<---");
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (Ada.Exceptions.Exception_Information (E));
      Put_Line ("---8<---Exception end----8<---");
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
      Log (Ada.Exceptions.Exception_Name (E), Level);
      Log (Ada.Exceptions.Exception_Message (E), Level);
      Log (Ada.Exceptions.Exception_Information (E), Level);
      if Reraise then
         Log ("Reraising", Level);
         Ada.Exceptions.Reraise_Occurrence (E);
      end if;
   end Report;

end Rx.Debug;
