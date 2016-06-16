with Rx.Integers;
with Rx.Schedulers;

package Rx.Interval is

   function Create (Pause       : Duration := 1.0;
                    First_Pause : Duration := 1.0;
                    Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                    return Rx.Integers.Observable;
   --  Delay Until is used, so slow processing may mean trouble...

end Rx.Interval;
