with Rx.Debug;

package body Rx.Interval is

   Instance : aliased Observable;

   overriding
   procedure Run (E : in out Event) is
   begin
      E.Count := E.Count + 1;
      E.Sched.Schedule (E, Pause);
      E.Observer.OnNext (E.Count);
   exception
      when E : others =>
         Rx.Debug.Print (E);
   end Run;

   overriding
   procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class)
   is
   begin
      O.E.Sched       := Scheduler;
      O.E.Observer    := S;
      Scheduler.Schedule (O.E, First_Pause);
   end Subscribe;

begin
   Output.Instance := Instance'Access;
end Rx.Interval;
