with Rx.Debug;
--  with Rx.Debug.Observers;
with Rx.Std; use Rx.Std;
with Rx.Subscriptions;

package body Rx.Devel is

   use Rx.Std.Integers;
   use Rx.Std.Integer_To_String;
   use Rx.Std.Strings;

--     package Ints renames Std.Integers;
--     package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image); use IntChecker;

--     function Selfsum (I : Rx_Integer) return Integers.Observable'Class is (Just (I + I));

   --     Inf : Integer_To_String.Typed.Actions.Inflater1 := AAA'Access;

--     function Below (I : Rx_Integer) return Integers.Observable'Class is
--       (if I <= 1
--        then Integers.Empty
--        else Numeric.Integers.Range_Slice (1, I - 1));

   function Image (I : Rx_Integer) return Strings.Observable'Class is
      (Just (I'Img));

   procedure Run is
      Subs : Rx.Subscriptions.Subscription;
   begin
      Debug.Trace ("starting");

      Subs :=
        From ((1, 2, 3, 4, 5))
--          & Integer_To_String.Flat_Map (No_Op
--                                        & Std.Casts.To_String)
        & Flat_Map (Image'Access)
        & Subscribe;

      while Subs.Is_Subscribed loop
         delay 0.1;
      end loop;
   end Run;

end Rx.Devel;
