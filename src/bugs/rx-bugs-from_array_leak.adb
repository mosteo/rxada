with Rx.Integers; use Rx.Integers; use Rx.Integers.Observables;

procedure Rx.Bugs.From_Array_Leak is

   use IntCount;

begin
   for I in 1 .. 666 loop
      declare
         Ob  : constant Integers.Observable := Empty;
         Op  : constant Integers.Observables.Operator := Count (First => 0);
         Ob2 : constant Integers.Observable :=
                 Ob
                 &
                 Op; -- Leak is around here, so caused by the & operator

         --  OTOH, valgrind is unable to pinpoint the leak
      begin
         null;
      end;
   end loop;
end Rx.Bugs.From_Array_Leak;
