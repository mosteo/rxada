with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package Rx.Debug is

   Enabled : constant Boolean := False;

   procedure Print (E : Ada.Exceptions.Exception_Occurrence);

   procedure Log (S : String); -- Prints S if Enabled is true

   procedure Put_Line (I : Integer);
   procedure Put_Line (S : String) renames Ada.Text_IO.Put_Line;

   function Image (T : Ada.Tags.Tag) return String renames Ada.Tags.Expanded_Name;

end Rx.Debug;
