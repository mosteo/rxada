with Ada.Exceptions;
with Ada.Tags;

with Gnat.IO;

with Gnat.Debug_Pools;

package Rx.Debug is

--     pragma Preelaborate;

   Enabled : constant Boolean := False;

   procedure Print (E : Ada.Exceptions.Exception_Occurrence);

   procedure Log (S : String); -- Prints S if Enabled is true

   procedure Put_Line (I : Integer);
   procedure Put_Line (S : String) renames Gnat.IO.Put_Line;

   function Image (T : Ada.Tags.Tag) return String renames Ada.Tags.Expanded_Name;

   --  Memory inspection
   Debug_Pool : Gnat.Debug_Pools.Debug_Pool;
   procedure Dump;

end Rx.Debug;
