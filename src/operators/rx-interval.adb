with Rx.Scheduler;

package body Rx.Interval is

   Instance : aliased Observable;

   type Event is new Rx.Scheduler.Runnable with record
      Count    : Positive;
      Observer : access Output.Observer'Class;
   end record;

   overriding
   procedure Run (E : Event) is
   begin
      Scheduler.Schedule (Event'(E.Count + 1, E.Observer), 1.0);
      E.Observer.OnNext (E.Count);
   end Run;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (O : in out Observable;
      S : access Output.Observer'Class)
   is
   begin
      Scheduler.Schedule (Event'(1, S), 1.0);
   end Subscribe;

begin
   Output.Instance := Instance'Access;
end Rx.Interval;
