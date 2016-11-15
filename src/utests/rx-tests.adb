with Rx.Debug;
with Rx.Debug.Observers;
with Rx.Errors;
with Rx.Operators;
with Rx.Std;
with Rx.Subscriptions;

package body Rx.Tests is

   use Rx.Std;

   package Ints renames Std.Integers;

   package StrToInt is new Rx.Operators (Strings, Integers);
   package IntToStr is new Rx.Operators (Integers, Strings);

   package IntCount is new Ints.Counters (Integer'Succ, 0);
   package StrCount is new StrToInt.Counters (Integer'Succ, 0);

   package IntChecker is new Debug.Observers (Std.Integers.Typedd, 0, Integer'Image);
   use IntChecker;

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
         Debug.Put_Line ("Verifying pass... " & Boolean'Image (I = Target));
      end Verify;

   end Verifier;

   package Verify_Int  is new Verifier (Integer, 1);
   package Verify_Str is new Verifier (String, "hello");
   procedure Assert_Int (V : Integer) renames Verify_Int.Verify;

   procedure Fail is
   begin
      raise Constraint_Error;
   end Fail;

   procedure Int_Fail (V : Integer) is
   begin
      raise Constraint_Error;
   end Int_Fail;

   procedure Int_Err_Pass (E : Rx.Errors.Occurrence) is null;
   Some_Error : Rx.Errors.Occurrence;

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

      Obs := +Defer (Deferred_Just'Access);
      Deferred := 1;
      Subs := Obs & Subscribe (Assert_Int'Access);
      -- Must receive the post-defer creation value (1)

      Subs := Integers.Empty & Count (1) & Subscribe (Assert_Int'Access);
      --  Should see zero items, hence a count of one

      Subscribe (Integers.Error (Some_Error),
                 On_Next      => Int_Fail'Access,
                 On_Completed => Fail'Access,
                 On_Error     => Int_Err_Pass'Access);
      --  Should only call On_Error and get a pass

      Subs :=
        IntEnums.Range_Count (First => 1, Count => 1) &
        Subscribe (Assert_Int'Access);

      Subs :=
        IntEnums.Range_Count (First => 1, Count => 10) &
--        Print (Tests.Image'Access) &
        Count (-9) &
        Subscribe (Assert_Int'Access);

      Subs :=
        IntEnums.Range_Slice (First => 1, Last => 10) &
        Count (-9) &
        Subscribe (Assert_Int'Access);

      Subs :=
        IntEnums.Range_Count (First => 1, Count => 0) &
        Count (1) &
        Subscribe (Assert_Int'Access);

      Subs :=
        IntEnums.Range_Slice (First => 1, Last => 0) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 0);

      Subs :=
        IntEnums.Range_Slice (First => 5, Last => 8) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 4,
                           Do_First => True, Ok_First => 5,
                           Do_Last  => True, Ok_Last  => 8);

      return Verify_Int.Passed;
   exception
      when others =>
         return False;
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

   type Subscriptor is new Integers.Subscriptor with null record;

   overriding procedure Do_On_Next (S : in out Subscriptor; V : Integer) is
      pragma Unreferenced (S);
   begin
      Debug.Put_Line ("In class");
      Assert_Int (V);
   end Do_On_Next;

   function Subscriptions return Boolean is
   begin
      --  Check subscription using tagged type
      Subs := Ints.Just (1) &
        Subscriptor'(Integers.Subscriptor with null record);

      --  Check unsubscription
      Subs :=
        Std.Interval (1) &
        Subscribe (Debug.Put_Line'Access);

      pragma Assert (Subs.Is_Subscribed);

      Subs.Unsubscribe;

      return Verify_Int.Passed and then not Subs.Is_Subscribed;
   exception
      when others =>
         return False;
   end Subscriptions;

end Rx.Tests;
