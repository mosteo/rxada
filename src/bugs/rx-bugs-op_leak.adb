with Rx.Integers; use Rx.Integers; use Rx.Integers.Observables;
with Rx.Subscriptions;

procedure Rx.Bugs.From_Array_Leak is

begin
   for I in 1 .. 666 loop
      declare
         Leak : constant Integers.Observable :=
                  No_Op
                  &
                  No_Op;

         --  OTOH, valgrind is unable to pinpoint the leak

      begin
         null;
      end;
   end loop;
end Rx.Bugs.From_Array_Leak;
