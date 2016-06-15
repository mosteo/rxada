with Rx.Integers; use Rx.Integers; use Rx.Integers.Observables;
with Rx.Debug; use Rx.Debug;
with Rx.Schedulers;

procedure Rx.Bugs.Op_Leak is

begin
   for I in 1 .. 1 loop
      Put_Line ("---8<---");
      declare
         Leak : Integers.Observable := No_Op & No_Op;
      begin
         Put_Line ("···");
      end;
      Put_Line ("--->8---");

--        Put_Line ("---8<---");
--        declare
--           No_Leak : Integers.Instance.Observables.Operator := -- This way it does not leak
--                    Integers.Instance.Observables.Operator (
--                    Integers.Observable'(
--                    No_Op & No_Op ));
--        begin
--           Put_Line ("···");
--        end;
--        Put_Line ("--->8---");
   end loop;

   Put_Line ("END");

--   Dump;
end Rx.Bugs.Op_Leak;
