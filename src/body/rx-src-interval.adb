with Ada.Calendar;

with Rx.Debug;
with Rx.Dispatchers;
with Rx.Impl.Shared_Observer;
with Rx.Src.Create;

package body Rx.Src.Interval is

   package Shared is new Rx.Impl.Shared_Observer (Typed);

   use Typed.Conversions;

   type Runner is new Dispatchers.Runnable with record
      Thread : Schedulers.Thread;
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
      RW.Thread.Schedule (RW, RW.Next);
   exception
      when No_Longer_Subscribed =>
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
      R : constant Runner := Runner'(Thread => S.Scheduler.Get_Thread,
                                     Pause  => S.Pause,
                                     Value  => S.First,
                                     Next   => Clock + S.First_Pause,
                                     Child  => Shared.Create (Observer));
   begin
      R.Thread.Schedule (R, Clock + S.First_Pause);
   end On_Subscribe;

   package Pre is new Src.Create (Typed);
   package Source is new Pre.With_State (State, On_Subscribe, Autocompletes => False);

   ------------
   -- Create --
   ------------

   function Create
     (First       : Typed.T;
                    Period       : Duration := 1.0;
                    First_Pause : Duration  := 0.0;
                    Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                    return Typed.Observable
   is
   begin
      return Source.Create (State'(First       => +First,
                                   Pause       => Period,
                                   First_Pause => First_Pause,
                                   Scheduler   => Scheduler));
   end Create;

end Rx.Src.Interval;
