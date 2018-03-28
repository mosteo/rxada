with Rx.Std; use Rx.Std;

use Rx.Std.Integers;

procedure Rx.Devel is

   function Inc (X : Rx_Integer) return Rx_Integer is (X + 1);

   S : constant Subscription :=
         Integers.From ((1, 2, 3, 4)) &
--         Integers.Map (Inc'Access) & -- Testing that this can't be done
         Integers.Subscribe;
begin
   null;
end Rx.Devel;
