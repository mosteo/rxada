with Rx.Integers;
with Rx.Operators;
with Rx.Strings;
with Rx.Subscriptions;

package body Rx.Tests is

   package Ints renames Integers.Observables;
   package Strs renames Strings.Observables;

   package StrToInt is new Rx.Operators (Strings.Observables, Integers.Observables);
   package IntToStr is new Rx.Operators (Integers.Observables, Strings.Observables);

   package IntCount is new Ints.Counters (Integer'Succ);
   package StrCount is new StrToInt.Counters (Integer'Succ);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);

   Chain : Subscriptions.No_Subscription;

   use Integers.Observables;
   use Strings.Observables;
   use StrToInt;
   use IntToStr;
   use IntCount;
   use StrCount;

   Passed : Boolean := True;

   generic
      type T (<>) is private;
      Target : T;
   procedure Verify (I : T);

   procedure Verify (I : T) is
   begin
      Passed := Passed and then I = Target;
   end Verify;

   procedure Verify_1 is new Verify (Integer, 1);

   procedure Verify_Hello is new Verify (String, "hello");

   -----------------
   -- Basic_Tests --
   -----------------

   function Basic_Tests return Boolean is
   begin
      Chain := Just (1) & Subscribe (Verify_1'Access);

      Chain := Just ("hello") & Subscribe (Verify_Hello'Access);

      Chain := Ints.From ((1, 1, 1)) & Subscribe (Verify_1'Access);

      Chain := Just ("Hello")
        &
        StrCount.Count (0)
        &
        Subscribe (Verify_1'Access);

      Chain := Ints.From ((1, 2))
        &
        Count (-1)
        &
        Subscribe (Verify_1'Access);

      -- Test counting reset
      declare
         Ob : constant Integers.Observable := Ints.From ((1, 2, 3, 4)) & Count (-3);
      begin
         Chain := Ob & Subscribe (Verify_1'Access);
         Chain := Ob & Subscribe (Verify_1'Access);
      end;

      return Passed;
   end Basic_Tests;

end Rx.Tests;
