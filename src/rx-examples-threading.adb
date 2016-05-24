with Ada.Text_IO; use Ada.Text_IO;

with Rx.Interval;
with Rx.Subscribe;

procedure Rx.Examples.Threading is

   procedure Put_Line (I : Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;

   package I1 is new Rx.Interval (1.0);
   package I2 is new Rx.Subscribe (I1.Output, Put_Line);

begin
   Put_Line ("Never reached");
end Rx.Examples.Threading;
