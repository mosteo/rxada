with Rx.Integers; use Rx.Integers; use Rx.Integers.Observables;
with Rx.Debug; use Rx.Debug;
with Rx.Schedulers;

procedure Rx.Bugs.Op_Leak is

begin
   for I in 1 .. 3 loop
      Put_Line ("---8<---");
      declare
         Leak : Integers.Instance.Observables.Operator :=
                  Integers.Instance.Observables.Operator (
                  Integers.Observable'(
                  No_Op
                  &
                  Observe_On (Schedulers.Immediate) ));
      begin
         Put_Line ("···");
      end;
      Put_Line ("--->8---");
   end loop;

   Put_Line ("END");

--   Dump;
end Rx.Bugs.Op_Leak;
