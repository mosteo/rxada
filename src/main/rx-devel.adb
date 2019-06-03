with Rx.Debug.Observers;
with Rx.Std; use Rx.Std; use Rx.Std.Integers;
with Rx.Subscriptions;

package body Rx.Devel is

   package Ints renames Std.Integers;
   package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image); use IntChecker;

   procedure Run is
      Subs : Rx.Subscriptions.Subscription with Unreferenced;
   begin
      Debug.Trace ("starting");

      Subs :=
        From ((1, 2, 3))
        & Ints.Flat_Map (Std.All_Positives'Access)
        & Std.Images.Integers.Print
        & Subscribe_Checker (Name     => "flatmap",
                             Do_Count => True, Ok_Count => 6);
   end Run;

end Rx.Devel;
