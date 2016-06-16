with Ada.Calendar;

with Rx.Dispatchers;
with Rx.Shared;
with Rx.Sources.Stateless;

package body Rx.Interval is

   package Shared is new Rx.Shared (Integers.Typed);

   type Runner is new Dispatchers.Runnable with record
      Sched  : Schedulers.Scheduler;
      Pause  : Duration;		 -- Repetitive period
      Value  : Integer := 0;	 	 -- Next integer to emit
      Next   : Ada.Calendar.Time;	 -- Reference for next deadline

      Child  : Shared.Observer; -- Reduce copy stress with a shared observer across runnables
   end record;

   ---------
   -- Run --
   ---------

   overriding procedure Run (R : in out Runner) is
      use Ada.Calendar;
   begin
      R.Child.On_Next (R.Value);
      R.Value := R.Value + 1;
      R.Next  := R.Next + R.Pause;
      R.Sched.Schedule (R, R.Next);
   end Run;

   type State is record
      Pause,
      First_Pause : Duration;
      Scheduler   : Schedulers.Scheduler;
   end record;

   ------------------
   -- On_Subscribe --
   ------------------

   procedure On_Subscribe (S : State; Observer : in out Integers.Typed.Observer) is
      use Ada.Calendar;
      R : Runner := (S.Scheduler,
                     S.Pause,
                     0,
                     Clock + S.First_Pause,
                     Shared.Create (Observer));
   begin
      S.Scheduler.Schedule (R, Clock + S.First_Pause);
   end On_Subscribe;

   package Source is new Sources.Stateless (Integers.Typed, State, On_Subscribe, Completes => False);

   ------------
   -- Create --
   ------------

   function Create
     (Pause       : Duration := 1.0;
      First_Pause : Duration := 1.0;
      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
      return Rx.Integers.Observable
   is
   begin
      return Source.Create (State'(Pause, First_Pause, Scheduler));
   end Create;

end Rx.Interval;
