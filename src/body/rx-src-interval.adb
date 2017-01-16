with Ada.Calendar;

with Rx.Debug;
with Rx.Dispatchers;
with Rx.Impl.Shared_Subscriber;
with Rx.Src.Create;
with Rx.Subscriptions;

package body Rx.Src.Interval is

   package Shared is new Rx.Impl.Shared_Subscriber (Typed);

   use Typed.Conversions;

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

   overriding procedure Run (R : Runner) is
      use Ada.Calendar;
      RW : Runner := R;
   begin
      RW.Child.On_Next (+R.Value);
      RW.Value := +Succ (+R.Value);
      RW.Next  := R.Next + R.Pause;
      RW.Sched.Schedule (RW, RW.Next);
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("Interval runner: caught No_Longer_Subscribed", Debug.Note);
      when E : others =>
         Typed.Defaults.Default_Error_Handler (RW.Child, E);
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
      R : constant Runner := (S.Scheduler,
                              S.Pause,
                              S.First,
                              Clock + S.First_Pause,
                              Shared.Create (Observer));
   begin
      S.Scheduler.Schedule (R, Clock + S.First_Pause);
   end On_Subscribe;

   package Pre is new Src.Create (Typed);
   package Source is new Pre.With_State (State, On_Subscribe, Autocompletes => False);

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
