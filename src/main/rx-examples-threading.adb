with Rx.Debug; use Rx.Debug;
with Rx.Integers;
with Rx.Interval;
with Rx.Schedulers;

procedure Rx.Examples.Threading is
   use Integers.Observables;
   use Strings.Observables;
   use IntToStr;
   use StrToInt;

begin
   Chain :=
     Interval.Create
--       & Print
--     & Subscribe_On (Schedulers.Computation)
--       & Observe_On (Schedulers.Background)
--       & Print
--       & Observe_On (Schedulers.IO)
--       & Print
--       & Observe_On (Schedulers.Computation)
--       & Print
     & Subscribe (Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
