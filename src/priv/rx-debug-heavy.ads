with Ada.Exceptions;

with Gnat.Debug_Pools;

package Rx.Debug.Heavy is

   --  For heavyweight non-preelaborable debugging

   pragma Elaborate_Body;

   procedure Backtrace (E : Ada.Exceptions.Exception_Occurrence);

   procedure Current_Backtrace (Bailout   : Boolean := False;
                                Exit_Code : Integer := 1);
   --  Print backtrace at point of call
   --  Optionally exit with exit code

   --  Memory inspection
   Debug_Pool : Gnat.Debug_Pools.Debug_Pool;
   procedure Dump;

end Rx.Debug.Heavy;
