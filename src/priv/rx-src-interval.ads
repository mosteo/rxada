with Rx.Schedulers;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>); -- Items emitted
   with function Succ (V : Typed.T) return Typed.T; -- Next in sequence
package Rx.Src.Interval is

   pragma Elaborate_Body;

   function Create (First       : Typed.T;
                    Period      : Duration := 1.0;
                    First_Pause : Duration := 0.0;
                    Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                    return Typed.Observable;
   --  Delay Until is used, so slow processing may mean trouble...

end Rx.Src.Interval;
