with Ada.Calendar;

with Rx.Dispatchers;
with Rx.Shared;
with Rx.Src.Stateless;

package body Rx.Src.Interval is

   package Shared is new Rx.Shared (Typed);

   use Typed.Type_Traits;

   type Runner is new Dispatchers.Runnable with record
      Sched  : Schedulers.Scheduler;
      Pause  : Duration;		 -- Repetitive period
      Value  : Typed.D;	   	 	 -- Next value to emit
      Next   : Ada.Calendar.Time;	 -- Reference for next deadline

      Child  : Shared.Observer; -- Reduce copy stress with a shared observer across runnables
   end record;

   ---------
   -- Run --
   ---------

   overriding procedure Run (R : in out Runner) is
      use Ada.Calendar;
   begin
      R.Child.On_Next (+R.Value);
      R.Value := +Succ (+R.Value);
      R.Next  := R.Next + R.Pause;
      R.Sched.Schedule (R, R.Next);
   end Run;

   type State is record
      First       : Typed.D;
      Pause,
      First_Pause : Duration;
      Scheduler   : Schedulers.Scheduler;
   end record;

   ------------------
   -- On_Subscribe --
   ------------------

   procedure On_Subscribe (S : State; Observer : in out Typed.Observer) is
      use Ada.Calendar;
      R : Runner := (S.Scheduler,
                     S.Pause,
                     S.First,
                     Clock + S.First_Pause,
                     Shared.Create (Observer));
   begin
      S.Scheduler.Schedule (R, Clock + S.First_Pause);
   end On_Subscribe;

   package Source is new Sources.Stateless (Typed, State, On_Subscribe, Completes => False);

   ------------
   -- Create --
   ------------

   function Create
     (First       : Typed.T;
                    Pause       : Duration := 1.0;
                    First_Pause : Duration := 1.0;
                    Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                    return Typed.Observable
   is
   begin
      return Source.Create (State'(+First, Pause, First_Pause, Scheduler));
   end Create;

end Rx.Src.Interval;
