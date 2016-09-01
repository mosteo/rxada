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

   generic
      type T (<>) is private;
      Target : T;
   package Verifier is

      Passed : Boolean := True;
      pragma Atomic (Passed);

      procedure Verify (I : T);

   end Verifier;

   package body Verifier is

      procedure Verify (I : T) is
      begin
         Passed := Passed and then I = Target;
      end Verify;

   end Verifier;

   -----------------
   -- Basic_Tests --
   -----------------

   package Verify_Basic_1  is new Verifier (Integer, 1);
   package Verify_Basic_Hi is new Verifier (String, "hello");

   function Basic_Tests return Boolean is
   begin
      Chain := Just (1) & Subscribe (Verify_Basic_1.Verify'Access);

      Chain := Just ("hello") & Subscribe (Verify_Basic_Hi.Verify'Access);

      Chain := Ints.From ((1, 1, 1)) & Subscribe (Verify_Basic_1.Verify'Access);

      Chain := Just ("Hello")
        &
        StrCount.Count (0)
        &
        Subscribe (Verify_Basic_1.Verify'Access);

      Chain := Ints.From ((1, 2))
        &
        Count (-1)
        &
        Subscribe (Verify_Basic_1.Verify'Access);

      -- Test counting reset
      declare
         Ob : constant Integers.Observable := Ints.From ((1, 2, 3, 4)) & Count (-3);
      begin
         Chain := Ob & Subscribe (Verify_Basic_1.Verify'Access);
         Chain := Ob & Subscribe (Verify_Basic_1.Verify'Access);
      end;

      return Verify_Basic_1.Passed and Verify_Basic_Hi.Passed;
   end Basic_Tests;

   -----------
   -- No_Op --
   -----------

   package No_Op_Check is new Verifier (Integer, 1);
   function No_Op return Boolean is
   begin
      Chain :=
        Ints.Just (1) &
        Ints.No_Op &
        Subscribe (No_Op_Check.Verify'Access);

      return No_Op_Check.Passed;
   end No_Op;

end Rx.Tests;
