with GNAT.Traceback.Symbolic;

package body Rx.Debug.Heavy is

   ----------
   -- Dump --
   ----------

   procedure Dump is
   begin
      Gnat.Debug_Pools.Print_Info_Stdout (Debug_Pool, Display_Leaks => True);
      Gnat.Debug_Pools.Dump_Gnatmem (Debug_Pool, "gmem.out");
   end Dump;

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

end Rx.Debug.Heavy;
