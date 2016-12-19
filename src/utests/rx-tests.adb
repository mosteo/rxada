with Rx.Actions;
with Rx.Debug;
with Rx.Debug.Observers;
with Rx.Errors;
with Rx.Indefinites;
with Rx.Std;
with Rx.Subscriptions;

package body Rx.Tests is

   use Rx.Std;

   package Ints renames Std.Integers;

   function String_Image (S : String) return String is (S);

   function Is_Even (I : Rx_Integer) return Boolean is (I mod 2 = 0);

   package FltChecker is new Debug.Observers (Std.Floats.Typed, 0.0, Rx_Float'Image);
   package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image); use IntChecker;
   package StrChecker is new Debug.Observers (Std.Strings.Typed, "", String_Image);     use StrChecker;

   Subs : Rx.Subscriptions.Subscription;

   use Floats;
   use Integers;
   use Strings;

   use Float_To_Integer;
   use Float_To_String;
   use Integer_To_Float;
   use Integer_To_String;
   use String_To_Integer;

   procedure Fail is
   begin
      raise Constraint_Error;
   end Fail;

   procedure Int_Fail (V : Rx_Integer) is
   begin
      raise Constraint_Error;
   end Int_Fail;

   procedure Int_Err_Pass (E : Rx.Errors.Occurrence) is null;
   Some_Error : Rx.Errors.Occurrence;

   function Is_Zero (V : Rx_Integer) return Boolean is (V = 0);
   function Is_One  (V : Rx_Integer) return Boolean is (V = 1);

   -------------
   -- Sources --
   -------------

   Deferred : Rx_Integer := 0;
   function Deferred_Just return Integers.Observable is (Integers.Just (Deferred));

   function Start_With_42 return Rx_Integer is (42);

   function Sources return Boolean is
      Obs : Integers.Definite_Observable;
   begin

      Subs := Just (1) & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                                            Do_First => True, Ok_First => 1,
                                            Do_Last  => True, Ok_Last  => 1);

      Subs := Just ("hello") & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                                                  Do_First => True, Ok_First => "hello",
                                                  Do_Last  => True, Ok_Last  => "hello");

      Subs := Just (Deferred) & Filter (Is_Zero'Access) & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                                                                             Do_First => True, Ok_First => 0,
                                                                             Do_Last  => True, Ok_Last  => 0);
      --  Should see a zero, pass the filter, count it and assert 1 as final result

      Subs := Ints.From ((1, 2, 3)) & Subscribe_Checker (Do_Count => True, Ok_Count => 3,
                                                         Do_First => True, Ok_First => 1,
                                                         Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Just ("Hello") &
        Numeric.Str_To_Int.Count &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      Subs := Ints.From ((1, 2)) &
        Numeric.Integers.Count &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 2,
                           Do_Last  => True, Ok_Last  => 2);

      Obs := +Defer (Deferred_Just'Access);
      Deferred := 1;
      Subs := Obs & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                                       Do_First => True, Ok_First => 1,
                                       Do_Last  => True, Ok_Last  => 1);
      -- Must receive the post-defer creation value (1)

      Subs := Integers.Empty & Subscribe_Checker (Do_Count => True, Ok_Count => 0);
      --  Should see zero items

      Subscribe (Integers.Error (Some_Error),
                 On_Next      => Int_Fail'Access,
                 On_Completed => Fail'Access,
                 On_Error     => Int_Err_Pass'Access);
      --  Should only call On_Error and get a pass

      Subs :=
        Numeric.Integers.Range_Count (First => 1, Count => 1) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      Subs :=
        Numeric.Integers.Range_Count (First => 1, Count => 10) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 10,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 10);

      Subs :=
        Numeric.Integers.Range_Slice (First => 1, Last => 10) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 10,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 10);

      Subs :=
        Numeric.Integers.Range_Count (First => 1, Count => 0) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 0);

      Subs :=
        Numeric.Integers.Range_Slice (First => 1, Last => 0) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 0);

      Subs :=
        Numeric.Integers.Range_Slice (First => 5, Last => 8) &
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

      return True;
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
      -- Test counting reset
      declare
         Ob : constant Integers.Observable := Ints.From ((1, 2, 3, 4)) & Numeric.Integers.Count;
      begin
         Subs := Ob & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                                         Do_First => True, Ok_First => 4,
                                         Do_Last  => True, Ok_Last  => 4);
         --  Both should report the same count
         Subs := Ob & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                                         Do_First => True, Ok_First => 4,
                                         Do_Last  => True, Ok_Last  => 4);
      end;


      -- Test limit
      Subs :=
        Ints.From ((1, 3, 2)) &
        Limit (2) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 2,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Ints.From ((1, 2, 3)) &
        Limit (5) & -- Check proper completion when not enough
        Subscribe_Checker (Do_Count => True, Ok_Count => 3,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      -- Filter test
      Subs := Ints.From ((2, 2, 1)) &
        Filter (Is_One'Access) &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      -- Repeats
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
        IntChecker.Subscribe (Do_Count => True, Ok_Count => 3);

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
      Subs := Just (1.0) & Casts.To_Integer & IntChecker.Subscribe (Do_Last => True, Ok_Last => 1);
      Subs := Just (1)   & Casts.To_Float   & FltChecker.Subscribe (Do_Last => True, Ok_Last => 1.0);
      Subs := Just (1)   & Casts.To_String  & StrChecker.Subscribe (Do_Last => True, Ok_Last => "1");

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

      -- No_Op
      Subs :=
        Just (1) &
        No_Op &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      -- Buffering
      Subs :=
        Numeric.Integers.Range_Count (1, 101) &
        Buffer (10) &
--          Std.Int_Images.Print &
        Numeric.Integers.Count &
--          Std.Int_Images.Print &
        Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 11,
                           Do_Last  => True, Ok_Last  => 11);

      -- Counting list sizes
      Subs :=
        Numeric.Integers.Range_Count (1, 11) &
        Buffer (10) &
--          Std.Int_Images.Print &
        Numeric.Integers.Length &
--          Std.Int_Images.Print &
        Subscribe_Checker (Do_Count => True, Ok_Count => 2,
                           Do_First => True, Ok_First => 10,
                           Do_Last  => True, Ok_Last  => 1);

      -- Splitting lists into elements
      Subs :=
        Numeric.Integers.Range_Count (1, 101) &
        Buffer (10) &
        Split &
        Buffer (7) &
        Flat_Map & -- This kind of flatmap is equivalent to split
        Subscribe_Checker (Do_Count => True, Ok_Count => 101,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 101);

      -- Debouncing
      Subs :=
        Numeric.Integers.Range_Slice (1, 3)
        & Debounce (0.2) -- Should let pass only the last one
        & Subscribe_Checker (Do_Count => True, Ok_Count => 1,
                             Do_First => True, Ok_First => 3,
                             Do_Last  => True, Ok_Last  => 3);

      return True;
   exception
      when E : others =>
         Debug.Print (E);
         return False;
   end Operators;

   ----------------
   -- Misc_Tests --
   ----------------

   function Misc_Tests return Boolean is
   begin
      return True;
   exception
      when others =>
         return False;
   end Misc_Tests;

   -------------------
   -- Subscriptions --
   -------------------

   function Subscriptions return Boolean is
   begin

      --  Check unsubscription
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

   -------------------
   -- Check_Linking --
   -------------------

   procedure Check_Linking with Unreferenced is
      package Rx_Str is new Rx.Indefinites (String);
      S : Rx.Subscriptions.Subscription;
      pragma Unreferenced (S);
      use Rx_Str.Observables.Linkers;
   begin
      S :=
        Rx_Str.Observables.Just ("Just this")
        &
        Rx_Str.Observables.Subscribe;
   end Check_Linking;

end Rx.Tests;
