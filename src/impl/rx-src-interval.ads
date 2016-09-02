with Rx.Schedulers;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>); -- Items emitted
   with function Succ (V : Typed.T) return Typed.T; -- Next in sequence
package Rx.Interval is

   function Create (First       : Typed.T;
                    Pause       : Duration := 1.0;
                    First_Pause : Duration := 1.0;
                    Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                    return Typed.Observable;
   --  Delay Until is used, so slow processing may mean trouble...

end Rx.Interval;
