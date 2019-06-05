with Rx.Debug;
--  with Rx.Debug.Observers;
with Rx.Schedulers;
with Rx.Std; use Rx.Std;
with Rx.Subscriptions;

package body Rx.Devel is

   use Rx.Std.Integers;
--     use Rx.Std.Integer_To_String;
--     use Rx.Std.Strings;

--     package Ints renames Std.Integers;
--     package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image); use IntChecker;

   function Selfsum (I : Rx_Integer) return Integers.Observable'Class is (Just (I + I));

--     Inf : Integer_To_String.Typed.Actions.Inflater1 := AAA'Access;

   procedure Run is
      Subs : Rx.Subscriptions.Subscription with Unreferenced;
   begin
      Debug.Trace ("starting");

--        Subs :=
--          Just (1)
--          & Expand (Selfsum'Access)
--          & Limit (16)
--          & Images.Integers.Print
--          & Subscribe;

      Subs :=
        Numeric.Integers.Range_Slice (1, 5)
        & Flat_Map (Repeat (4)
                    & Observe_On (Schedulers.Computation)
                    & Hold (Fixed => 0.0, Random => 0.01))
        & Images.Integers.Print
        & Subscribe;
   end Run;

end Rx.Devel;
