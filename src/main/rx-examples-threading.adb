with Rx.Debug; use Rx.Debug;
with Rx.Std;   use Rx.Std;
with Rx.Schedulers;
with Rx.Schedulers.Pools;

procedure Rx.Examples.Threading is
   use Integers;
   use Strings;
   use IntToStr;
   use StrToInt;

   Custom_Pool : Schedulers.Pools.Pool := Schedulers.Pools.Create (Size => 2);

begin
   Sub :=
     Std.Interval
     & Print
     & Subscribe_On (Schedulers.IO)
     & Observe_On (Schedulers.Idle_Thread)
     & Print
     & Observe_On (Schedulers.New_Thread)
     & Print
     & Observe_On (Schedulers.Computation)
     & Print
     & Observe_On (Custom_Pool.Get_Next)
     & Print
     & Observe_On (Custom_Pool.Get_Next)
     & Print
     & Observe_On (Custom_Pool.Get_Idle)
     & Print
     & Subscribe (Put_Line'Access);

   delay 5.0;
   Schedulers.Shutdown;
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
