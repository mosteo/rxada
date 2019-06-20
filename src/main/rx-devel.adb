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

--     function Selfsum (I : Rx_Integer) return Integers.Observable'Class is (Just (I + I));

   --     Inf : Integer_To_String.Typed.Actions.Inflater1 := AAA'Access;

   function Below (I : Rx_Integer) return Integers.Observable'Class is
     (if I <= 1
      then Integers.Empty
      else Numeric.Integers.Range_Slice (1, I - 1));

   procedure Run is
      Subs : Rx.Subscriptions.Subscription;
   begin
      Debug.Trace ("starting");

      declare
         Only_Strings : constant Rx.Std.Strings.Observable'Class :=
                          Rx.Std.Strings.No_Op
                          & Rx.Std.Strings.No_Op
                          & Rx.Std.Strings.No_Op;

         Ints_And_Strings : constant Rx.Std.Strings.Observable'Class :=
                              Rx.Std.Integers.No_Op
                              & Rx.Std.Integers.No_Op
                              & Rx.Std.Casts.To_String
                              & Rx.Std.Strings.No_Op
                              & Rx.Std.Strings.No_Op;
      begin
         Rx.Std.Integer_To_String.Diagnose (Only_Strings);
         Debug.Put_Line ("---");
         Rx.Std.Integer_To_String.Diagnose (Ints_And_Strings);
      end;

      while Subs.Is_Subscribed loop
         delay 0.1;
      end loop;
   end Run;

end Rx.Devel;
