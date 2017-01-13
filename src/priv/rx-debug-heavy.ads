with Ada.Exceptions;

with Gnat.Debug_Pools;

package Rx.Debug.Heavy is

   --  For heavyweight non-preelaborable debugging

   pragma Elaborate_Body;

   procedure Backtrace (E : Ada.Exceptions.Exception_Occurrence);

   --  Memory inspection
   Debug_Pool : Gnat.Debug_Pools.Debug_Pool;
   procedure Dump;

end Rx.Debug.Heavy;
