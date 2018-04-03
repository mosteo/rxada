with Rx.Std; use Rx.Std;

use Rx.Std.Integers;

procedure Rx.Devel is

   function Inc (X : Rx_Integer) return Rx_Integer is (X + 1);

   S : constant Subscription :=
         Integers.From ((1, 2, 3, 4)) &
         Integers.Map (Inc'Unrestricted_Access) & -- Testing that this can't be done with checked access
         Inc'Unrestricted_Access &                -- Alternate Map with &
         Images.Integers.Print &
         Integers.Subscribe
   with Unreferenced;
begin
   Integers.subscribe (Integers.Just (1) & Images.Integers.Print);
end Rx.Devel;
