with Rx.Root;
with Rx.Scheduler;

package body Rx.Interval is

   --     Instance : aliased Observable;

   package Base is new Rx.Operator (Positive);

   type Event is new Rx.Scheduler.Runnable with record
      Count    : Positive;
      Observer : access Root.Observer'Class;
      Sched    : access Scheduler.Object'Class;
   end record;

   overriding
   procedure Run (E : Event);

   overriding
   procedure Run (E : Event) is
   begin
      E.Sched.Schedule (Event'(E.Count + 1, E.Observer, E.Sched), 1.0);
      Base.Observer'Class (E.Observer.all).OnNext (E.Count);
   end Run;

   package body Producer is

      overriding
      procedure Subscribe (O : in out Observable;
                           S : access Output.Observer'Class) is
      begin
         Scheduler.Schedule (Event'(1, S, Scheduler), 1.0);
      end Subscribe;

      Instance : aliased Observable;

   begin
      Output.Instance := Instance'Access;
   end Producer;

   ---------------
   -- Subscribe --
   ---------------

--     overriding
--     procedure Subscribe
--       (O : in out Observable;
--        S : access Base.Observer'Class)
--     is
--     begin
--        Scheduler.Schedule (Event'(1, S), 1.0);
--     end Subscribe;

end Rx.Interval;
