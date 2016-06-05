with Ada.Text_IO; use Ada.Text_IO;

with Rx.Debug;

procedure Rx.Examples.Threading is
begin
   -- Test;
   Put_Line ("After main");
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
