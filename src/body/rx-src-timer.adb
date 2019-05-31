with Rx.Impl.Preservers;
with Rx.Op.Limit;
with Rx.Src.Interval;

package body Rx.Src.Timer is

   function Dummy_Succ (V : Typed.T) return Typed.T is (V);

   package Operate    is new Rx.Impl.Preservers (Typed);
   package RxInterval is new Rx.Src.Interval (Typed, Dummy_Succ);
   package RxLimit    is new Rx.Op.Limit (Operate);

   ------------
   -- Create --
   ------------

   function Create
     (V         : Typed.T;
      After     : Duration;
      Scheduler : Schedulers.Scheduler := Schedulers.Computation)
      return Typed.Observable
   is
      use Operate.Linkers;
   begin
      return
        RxInterval.Create (First       => V,
                           Period       => 0.0,
                           First_Pause => After,
                           Scheduler   => Scheduler)
        & RxLimit.Create (1);
   end Create;

end Rx.Src.Timer;
