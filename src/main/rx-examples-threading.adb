with Rx.Debug; use Rx.Debug;
with Rx.Integers;
with Rx.Schedulers;

procedure Rx.Examples.Threading is
   use Integers.Observables;
   use Strings.Observables;
   use IntToStr;
   use StrToInt;

begin
   Chain :=
     Integers.Observables.From ((1, 2, 3, 4, 5))
     & Observe_On (Schedulers.Background)
     & Map (Image'Access)
     & Observe_On (Schedulers.IO)
     & Subscribe (Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
