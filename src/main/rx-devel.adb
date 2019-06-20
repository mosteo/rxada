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

--     function Selfsum (I : Rx_Integer) return Integers.Observable'Class is (Just (I + I));

   --     Inf : Integer_To_String.Typed.Actions.Inflater1 := AAA'Access;

   function Below (I : Rx_Integer) return Integers.Observable'Class is
     (if I <= 1
      then Integers.Empty
      else Numeric.Integers.Range_Slice (1, I - 1));

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

--        Subs :=
--          From ((1, 2))
--          & Flat_Map (No_Op)
--          & Images.Integers.Print
--          & Subscribe;

      Subs :=
        From ((1, 2, 3, 4))
        & Expand (Below'Access)
        & Images.Integers.Print
        & Subscribe;

      while Subs.Is_Subscribed loop
         delay 0.1;
      end loop;
   end Run;

end Rx.Devel;
