with Rx.Debug;
with Rx.Operators;
with Rx.Std;
with Rx.Subscriptions;

package body Rx.Tests is

   use Rx.Std;

   package Ints renames Std.Integers;
   package Strs renames Std.Strings;

   package StrToInt is new Rx.Operators (Strings, Integers);
   package IntToStr is new Rx.Operators (Integers, Strings);

   package IntCount is new Ints.Counters (Integer'Succ, 0);
   package StrCount is new StrToInt.Counters (Integer'Succ, 0);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);

   Subs : Rx.Subscriptions.Subscription;

   use Integers;
   use Strings;
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

   package Verify_Int  is new Verifier (Integer, 1);
   package Verify_Str is new Verifier (String, "hello");
   procedure Assert_Int (V : Integer) renames Verify_Int.Verify;
   procedure Assert_Str (V : String)  renames Verify_Str.Verify;

   function Is_Zero (V : Integer) return Boolean is (V = 0);
   function Is_One (V : Integer) return Boolean is (V = 1);

   -------------
   -- Sources --
   -------------

   Deferred : Integer := 0;
   function Deferred_Just return Integers.Observable is (Integers.Just (Deferred));

   function Sources return Boolean is
      Obs : Integers.Defob;
   begin
      Verify_Int.Passed := True;

      Subs := Just (Deferred) & Filter (Is_Zero'Access) & Count & Subscribe (Assert_Int'Access);
      --  Should see a zero, pass the filter, count it and assert 1 as final result

      Obs := + Defer (Deferred_Just'Access);
      Deferred := 1;
      Subs := Obs & Subscribe (Assert_Int'Access); -- Must receive the post-defer creation value (1)

      return True;
   end Sources;

   ---------------
   -- Operators --
   ---------------

   function Operators return Boolean is
   begin
      return True;
   end Operators;

   -----------------
   -- Basic_Tests --
   -----------------

   function Basic_Tests return Boolean is
   begin
      Verify_Int.Passed := True;
      Verify_Str.Passed := True;

      Subs := Just (1) & Subscribe (Verify_Int.Verify'Access);

      Subs := Just ("hello") & Subscribe (Verify_Str.Verify'Access);

      Subs := Ints.From ((1, 1, 1)) & Subscribe (Verify_Int.Verify'Access);

      Subs := Just ("Hello")
        &
        StrCount.Count (0)
        &
        Subscribe (Verify_Int.Verify'Access);

      Subs := Ints.From ((1, 2))
        &
        Count (-1)
        &
        Subscribe (Verify_Int.Verify'Access);

      -- Test counting reset
      declare
         Ob : constant Integers.Observable := Ints.From ((1, 2, 3, 4)) & Count (-3);
      begin
         Subs := Ob & Subscribe (Verify_Int.Verify'Access);
         Subs := Ob & Subscribe (Verify_Int.Verify'Access);
      end;

      -- Test limit
      Subs := Ints.From ((1, 1, 2))
        &
        Limit (2)
        &
        Subscribe (Verify_Int.Verify'Access);

      Subs := Ints.From ((1, 1, 2))
        &
        Limit (2)
        &
        Count (-1)
        &
        Subscribe (Verify_Int.Verify'Access);

      Subs := Ints.From ((1, 1, 1))
        &
        Limit (5) -- Check proper completion when not enough
        &
        Subscribe (Verify_Int.Verify'Access);

      -- Filter test
      Subs := Ints.From ((2, 2, 1)) &
        Filter (Is_One'Access) &
        Subscribe (Assert_Int'Access);

      Subs := Ints.From ((1, 2, 2)) &
        Filter (Is_One'Access) &
        Count (0) &
        Subscribe (Assert_Int'Access);

      return Verify_Int.Passed and Verify_Str.Passed;
   end Basic_Tests;

   -----------
   -- No_Op --
   -----------

   package No_Op_Check is new Verifier (Integer, 1);
   function No_Op return Boolean is
   begin
      Subs :=
        Ints.Just (1) &
        Ints.No_Op &
        Subscribe (No_Op_Check.Verify'Access);

      return No_Op_Check.Passed;
   end No_Op;

   -------------------
   -- Subscriptions --
   -------------------

   function Subscriptions return Boolean is
   begin
      Subs :=
        Std.Interval (1) &
        Subscribe (Debug.Put_Line'Access);

      pragma Assert (Subs.Is_Subscribed);

      Subs.Unsubscribe;

      return not Subs.Is_Subscribed;
   exception
      when others =>
         return False;
   end Subscriptions;

end Rx.Tests;
