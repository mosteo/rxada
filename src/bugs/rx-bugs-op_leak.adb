with Rx.Integers; use Rx.Integers; use Rx.Integers.Observables;
with Rx.Debug; use Rx.Debug;

procedure Rx.Bugs.Op_Leak is

begin
   for I in 1 .. 3 loop
      Put_Line ("---8<---");
      declare
         Leak : Integers.Observable :=
                  No_Op
                  &
                  No_Op;
         pragma Unreferenced (Leak);
      begin
         null;
      end;
      Put_Line ("--->8---");
   end loop;

   Put_Line ("END");

--   Dump;
end Rx.Bugs.Op_Leak;
