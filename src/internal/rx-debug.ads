with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package Rx.Debug is

   procedure Print (E : Ada.Exceptions.Exception_Occurrence);

   procedure Put_Line (I : Integer);
   procedure Put_Line (S : String) renames Ada.Text_IO.Put_Line;

end Rx.Debug;
