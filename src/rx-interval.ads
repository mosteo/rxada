with Rx.Operator;
with Rx.Scheduler;
with Rx.Schedulers;

generic
   Pause       : Duration := 1.0;
   First_Pause : Duration := Pause;
   Scheduler   : Rx.Schedulers.Object := Rx.Schedulers.Background;
package Rx.Interval is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (Integer);

private

   --  This should be private but a nested child package doesn't allow it
   type Event is new Rx.Scheduler.Runnable with record
      Count    : Natural := 0;
      Observer : access Output.Observer'Class;
      Sched    : access Rx.Scheduler.Object'Class;
   end record;

   overriding
   procedure Run (E : in out Event);
   --  End of private

   type Observable is limited new Output.Observable with record
      E : Event;
   end record;

   overriding
   procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class);

end Rx.Interval;
