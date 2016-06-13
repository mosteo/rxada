with Rx.Integers; use Rx.Integers;

procedure Rx.Bugs.From_Array_Leak is

   use IntCount;

begin
   for I in 1 .. 666 loop
      declare
         Ob  : constant Integers.Observable := Integers.Observables.From ((0, 1, 2, 3));
         Op  : constant Integers.Observables.Operator := Count (First => 0);
         Ob2 : constant Integers.Observable :=
                 Ob
                 &
                 Op; -- Leak is around here, but apparently caused by the arrays in From (according to gnatmem)
         --  OTOH, valgrind is unable to pinpoint the leak
      begin
         null;
      end;
   end loop;
end Rx.Bugs.From_Array_Leak;
