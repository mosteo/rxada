with Ada.Text_IO; use Ada.Text_IO;

with Rx.Debug;
with Rx.Interval;
with Rx.Subscribe;

procedure Rx.Examples.Threading is

   procedure Main is
      package I1 is new Rx.Interval.Producer (1.0);
      package I2 is new Rx.Subscribe (I1.Output, Debug.Put_Line);
   begin
      null;
   end Main;


begin
   Main;
   Put_Line ("After main");
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
