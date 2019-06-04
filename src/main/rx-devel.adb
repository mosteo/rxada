with Rx.Debug;
--  with Rx.Debug.Observers;
with Rx.Schedulers;
with Rx.Std; use Rx.Std;
with Rx.Subscriptions;

package body Rx.Devel is

   use Rx.Std.Integers;
   use Rx.Std.Integer_To_String;
   use Rx.Std.Strings;

--     package Ints renames Std.Integers;
--     package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image); use IntChecker;

   function AAA (I : Rx_Integer) return Strings.Observable'Class is
     (Strings.Just (String'(1 .. Integer (I) => 'a')));

--     Inf : Integer_To_String.Typed.Actions.Inflater1 := AAA'Access;

   procedure Run is
      Subs : Rx.Subscriptions.Subscription with Unreferenced;
   begin
      Debug.Trace ("starting");

      Subs :=
        From ((1, 2, 3, 4, 5))
        & Integer_To_String.Flat_Map (AAA'Access,
                                      Observe_On (Schedulers.Computation)
                                      & Map (Std.String_Succ'Access))
        & Std.Images.Strings.Print
        & Subscribe;
   end Run;

end Rx.Devel;
