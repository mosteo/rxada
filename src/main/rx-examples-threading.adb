with Rx.Debug; use Rx.Debug;
with Rx.Integers;
with Rx.Standard;
with Rx.Schedulers;

procedure Rx.Examples.Threading is
   use Integers.Observables;
   use Strings.Observables;
   use IntToStr;
   use StrToInt;

begin
   Chain :=
     Standard.Interval
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
