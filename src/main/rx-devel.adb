with Rx.Debug.Observers;
with Rx.Schedulers;
with Rx.Std; use Rx.Std; use Rx.Std.Integers;
with Rx.Subscriptions;

package body Rx.Devel is

   package Ints renames Std.Integers;
   package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image); use IntChecker;

   function Swallow (Unused : Rx.Rx_Integer) return Ints.Observable is (Ints.Empty);

   procedure Run is
      Subs : Rx.Subscriptions.Subscription with Unreferenced;
   begin
      Debug.Trace ("starting");

      Subs :=
        Ints.Empty
        & Ints.Flat_Map (Std.All_Positives'Access)
        & Std.Images.Integers.Print
        & Subscribe_Checker (Name     => "flatmap empty master",
                             Do_Count => True, Ok_Count => 0);

      Subs :=
        Ints.From ((1, 2, 3))
        & Ints.Flat_Map (Swallow'Access)
        & Std.Images.Integers.Print
        & Subscribe_Checker (Name     => "flatmap empty subs",
                             Do_Count => True, Ok_Count => 0);

      Subs :=
        Std.Numeric.Integers.Range_Slice (1, 4)
        & Ints.Flat_Map (Std.All_Positives'Access)
        & Std.Images.Integers.Print
        & Subscribe_Checker (Name     => "flatmap immediate",
                             Do_Count => True, Ok_Count => 10);

      Subs :=
        Std.Numeric.Integers.Range_Slice (1, 5)
        & Ints.Flat_Map (Repeat (9)
                        & Observe_On (Schedulers.Computation)
                        & Hold (Fixed => 0.0, Random => 0.1)
                        )
        & Print
        & Std.Images.Integers.Print
        & Subscribe_Checker (Name     => "flatmap w pipeline & scheduler",
                             Do_Count => True, Ok_Count => 50,
                             Period   => 5.0);

      Subs :=
        From ((1, 2, 3))
        & Observe_On (Schedulers.New_Thread)
        & Merge_With (From ((4, 5, 6))
                      & Observe_On (Schedulers.New_Thread))
        & Print
        & Std.Images.Integers.Print
        & Numeric.Integers.Count
        & Subscribe_Checker (Name     => "merge-with & count w scheduler",
                             Do_Count => True, Ok_Count => 1,
                             Do_Last  => True, Ok_Last  => 6);
   end Run;

end Rx.Devel;
