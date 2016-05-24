with Rx.Operator;

generic
   Pause       : Duration := 1.0;
   First_Pause : Duration := Pause;
package Rx.Interval is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (Positive);

private

   type Observable is new Output.Observable with record
      Next : Positive := 1;
   end record;

   overriding
   procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class);

end Rx.Interval;
