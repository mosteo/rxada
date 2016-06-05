with Rx.Debug;

package body Rx.Scheduler.Immediate is

   --------------
   -- Schedule --
   --------------

   overriding procedure Schedule
     (Where : in out Object;
      What : in out Runnable'Class;
      After : Duration := 0.0)
   is
   begin
      delay After;
      What.Run;
   exception
      when E : others =>
         Rx.Debug.Print (E);
   end Schedule;

end Rx.Scheduler.Immediate;
