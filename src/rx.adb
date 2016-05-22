with Ada.Text_IO; use Ada.Text_IO;

package body Rx is

   procedure Writeln (I : Integer) is
   begin
      Put_Line (I'Img);
   end Writeln;

   procedure Debug (S : String) is
   begin
      Put_Line (S);
   end Debug;

end Rx;
