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


   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Debug.Print (E);
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Backtrace;

end Rx.Debug.Heavy;
