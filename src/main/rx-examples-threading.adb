with Rx.Debug; use Rx.Debug;
with Rx.Std;
with Rx.Schedulers;

procedure Rx.Examples.Threading is
   use Integers;
   use Strings;
   use IntToStr;
   use StrToInt;

begin
   Chain :=
     Std.Interval
     & Print
     & Subscribe_On (Schedulers.Computation)
     & Observe_On (Schedulers.Background)
     & Print
     & Observe_On (Schedulers.IO)
     & Print
     & Observe_On (Schedulers.Computation)
     & Print
     & Subscribe (Put_Line'Access);

   delay 5.0;
   Schedulers.Shutdown;
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
