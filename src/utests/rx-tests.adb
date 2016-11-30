with Rx.Actions;
with Rx.Debug;
with Rx.Debug.Observers;
with Rx.Errors;
with Rx.Std;
with Rx.Subscriptions;

package body Rx.Tests is

   use Rx.Std;

   package Ints renames Std.Integers;

   function String_Image (S : String) return String is (S);

   function Is_Even (I : Integer) return Boolean is (I mod 2 = 0);

   package FltChecker is new Debug.Observers (Std.Floats.Typed, 0.0, Float'Image);
   package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Integer'Image); use IntChecker;
   package StrChecker is new Debug.Observers (Std.Strings.Typed, "", String_Image);

   Subs : Rx.Subscriptions.Subscription;

   use Floats;
   use Integers;
   use Strings;

   use FltToInt;
   use FltToStr;
   use IntToFlt;
   use IntToStr;
   use StrToInt;

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

   function Start_With_42 return Integer is (42);

   function Sources return Boolean is
      Obs : Integers.Definite_Observable;
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

      Subs :=
        Start (Start_With_42'Access) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 42,
                           Do_Last  => True, Ok_Last  => 42);

      Subs :=
        Timer (1, 0.1) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);
      Subs :=
        Timer (0.1) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 0,
                           Do_Last  => True, Ok_Last  => 0);

      return Verify_Int.Passed;
   exception
      when others =>
         return False;
   end Sources;

   ---------------
   -- Operators --
   ---------------

   function Never  return Boolean is (False);
   function Always return Boolean is (True);

   function Operators return Boolean is
      use Actions;
   begin
      Subs :=
        Just (1) &
        Repeat (9) & -- Standard repeating
        Subscribe_Checker (Do_Count => True, Ok_Count => 10,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      Subs :=
        Just (1) &
        While_Do (Never'Access) & -- Trivial instant exit
        Subscribe_Checker (Do_Count => True, Ok_Count => 0);

      Subs :=
        Just (1) &
        Repeat (20) &
        Take (10) & -- Repeat with limit
        Subscribe_Checker (Do_Count => True, Ok_Count => 10);

      Subs :=
        Just (1) &
        Repeat_Forever &
        Take (10) & -- Repeat forever with limit
        Subscribe_Checker (Do_Count => True, Ok_Count => 10);

      Subs :=
        Just (1) &
        Repeat (4) &
        Take (10) & -- Repeat with unused limit
        Subscribe_Checker (Do_Count => True, Ok_Count => 5);

      Subs :=
        From ((1, 2, 3)) &
        Repeat_Until (Always'Access) & -- Trivial exit after first repeat
        Subscribe_Checker (Do_Count => True, Ok_Count => 3);

      Subs :=
        Ints.From ((1, 2, 3)) &
        Repeat_Until (Actions.Count (Times => 3)) & -- Trivial exit after first repeat
        Subscribe_Checker (Do_Count => True, Ok_Count => 9,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Ints.From ((1, 2, 3)) &
        While_Do (not Actions.Count (Times => 3)) & -- Trivial exit after first repeat
        Subscribe_Checker (Do_Count => True, Ok_Count => 6,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      --  Casts
      Subs := Just (1.0) & Float_To_Integer  & IntChecker.Subscribe (Do_Last => True, Ok_Last => 1);
      Subs := Just (1)   & Integer_To_Float  & FltChecker.Subscribe (Do_Last => True, Ok_Last => 1.0);
      Subs := Just (1)   & Integer_To_String & StrChecker.Subscribe (Do_Last => True, Ok_Last => "1");

      --  Last
      Subs :=
        From ((1, 2, 3)) &
        Last &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 3,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Integers.Empty &
        Last_Or_Default (3) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 3,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        From ((1, 2, 3)) &
        Last (Is_Even'Access) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 2,
                           Do_Last  => True, Ok_Last  => 2);

      Subs :=
        From ((1, 3)) &
        Last_Or_Default (2, Is_Even'Access) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 2,
                           Do_Last  => True, Ok_Last  => 2);

      Subs :=
        From ((1, 4)) &
        Last_Or_Default (2, Is_Even'Access) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 4,
                           Do_Last  => True, Ok_Last  => 4);

      -- Buffering
      Subs :=
        IntEnums.Range_Count (1, 101) &
        Buffer (10) &
--          Std.Int_Images.Print &
        IntCount.Count &
--          Std.Int_Images.Print &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 11,
                           Do_Last  => True, Ok_Last  => 11);

      -- Counting list sizes
      Subs :=
        IntEnums.Range_Count (1, 11) &
        Buffer (10) &
--          Std.Int_Images.Print &
        Std.Length &
--          Std.Int_Images.Print &
        Subscribe_Checker (Do_Count => True, Ok_Count => 2,
                           Do_First => True, Ok_First => 10,
                           Do_Last  => True, Ok_Last  => 1);

      pragma Compile_Time_Warning (True, "Missing check for Serialize operation (need mixer observer)");

      return True;
   exception
      when E : others =>
         Debug.Print (E);
         return False;
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
