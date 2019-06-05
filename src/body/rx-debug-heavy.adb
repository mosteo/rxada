with GNAT.OS_Lib;
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

   -----------------------
   -- Current_Backtrace --
   -----------------------

   procedure Current_Backtrace (Bailout   : Boolean := False;
                                Exit_Code : Integer := 1) is
   begin
      Put_Line (Gnat.Traceback.Symbolic.Symbolic_Traceback (Gnat.Traceback.Call_Chain (Max_Len => 20)));
      if Bailout then
         GNAT.OS_Lib.OS_Exit (Exit_Code);
      end if;
   end Current_Backtrace;

end Rx.Debug.Heavy;
