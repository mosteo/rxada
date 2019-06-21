with Rx.Actions;
with Rx.Debug;
with Rx.Debug.Observers;
with Rx.Errors;
with Rx.Indefinites;
with Rx.Schedulers;
with Rx.Schedulers.Pools;
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
   -- use Float_To_String;
   use Integer_To_Float;
   use Integer_To_String;
   use String_To_Integer;

   Flag_On_Next : Boolean := False;
   procedure Test_On_Next (Unused : Rx_Integer) is
   begin
      Flag_On_Next := True;
   end Test_On_Next;

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

   function Swallow (Unused : Rx.Rx_Integer) return Ints.Observable is (Ints.Empty);

   type Emit_Range is new Ints.Operate.Operator with null record;

   overriding procedure On_Next (This : in out Emit_Range; V : Rx_Integer) is
   begin
      for I in 1 .. V loop
         This.Get_Observer.On_Next (I);
      end loop;
   end On_Next;

   Int_Emitter : constant Emit_Range := (Ints.Operate.Operator with null record);

   function AAA (I : Rx_Integer) return Strings.Observable'Class is
     (Strings.Just (String'(1 .. Integer (I) => 'a')));

   function Selfsum (I : Rx_Integer) return Integers.Observable'Class is (Just (I + I));

   function Below (I : Rx_Integer) return Integers.Observable'Class is
     (if I <= 1
      then Integers.Empty
      else Numeric.Integers.Range_Slice (1, I - 1));
   --  Recursively emit integers below given one. Used to test Expand with finite recursivity

   procedure Stopwatch_Test (Event_Kind         : Rx_Event_Kinds;
                             Unused             : Duration;
                             Since_Subscription : Duration)
   is
   begin
      if Event_Kind = On_Complete then
         pragma Assert (Since_Subscription >= 0.5, "Not enough time elapsed");
         pragma Assert (Since_Subscription <= 0.6, "Too much time elapsed");
      end if;
   end Stopwatch_Test;

   -----------------
   -- Custom Pool --
   -----------------

   Custom_Pool : Schedulers.Pools.Pool := Schedulers.Pools.Create (Size => 2, Name => "Custom");

   function Custom_Idle return Schedulers.Thread is (Custom_Pool.Get_Idle);
   function Custom_Next return Schedulers.Thread is (Custom_Pool.Get_Next);

   -------------
   -- Sources --
   -------------

   Deferred : Rx_Integer := 0;
   function Deferred_Just return Integers.Observable is (Integers.Just (Deferred));

   function Start_With_42 return Rx_Integer is (42);

   function Sources return Boolean is
      Obs : Integers.Definite_Observable;
   begin

      Subs := Just (1) & Subscribe_Checker (Name     => "just int",
                                            Do_Count => True, Ok_Count => 1,
                                            Do_First => True, Ok_First => 1,
                                            Do_Last  => True, Ok_Last  => 1);

      Subs := Just ("hello") & Subscribe_Checker (Name     => "just string",
                                                  Do_Count => True, Ok_Count => 1,
                                                  Do_First => True, Ok_First => "hello",
                                                  Do_Last  => True, Ok_Last  => "hello");

      Subs := Just (Deferred) & Filter (Is_Zero'Access) & Subscribe_Checker (Name     => "just & filter",
                                                                             Do_Count => True, Ok_Count => 1,
                                                                             Do_First => True, Ok_First => 0,
                                                                             Do_Last  => True, Ok_Last  => 0);
      --  Should see a zero, pass the filter, count it and assert 1 as final result

      Subs := Ints.From ((1, 2, 3)) & Subscribe_Checker (Name     => "from",
                                                         Do_Count => True, Ok_Count => 3,
                                                         Do_First => True, Ok_First => 1,
                                                         Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Just ("Hello") &
        Numeric.Str_To_Int.Count &
        Subscribe_Checker (Name     => "count str2int",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      Subs := Ints.From ((1, 2)) &
        Numeric.Integers.Count &
        Subscribe_Checker (Name     => "count integers",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 2,
                           Do_Last  => True, Ok_Last  => 2);

      Obs := +Defer (Deferred_Just'Access);
      Deferred := 1;
      Subs := Obs & Subscribe_Checker (Name     => "deferred",
                                       Do_Count => True, Ok_Count => 1,
                                       Do_First => True, Ok_First => 1,
                                       Do_Last  => True, Ok_Last  => 1);
      -- Must receive the post-defer creation value (1)

      Subs := Integers.Empty & Subscribe_Checker (Name     => "empty",
                                                  Do_Count => True, Ok_Count => 0);
      --  Should see zero items

      For_Each (Integers.Error (Some_Error),
                On_Next      => Int_Fail'Access,
                On_Complete  => Fail'Access,
                On_Error     => Int_Err_Pass'Access);
      --  Should only call On_Error and get a pass

      Subs :=
        Numeric.Integers.Range_Count (First => 1, Count => 1) &
        Subscribe_Checker (Name     => "range_count 1",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      Subs :=
        Numeric.Integers.Range_Count (First => 1, Count => 10) &
        Subscribe_Checker (Name     => "range_count 10",
                           Do_Count => True, Ok_Count => 10,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 10);

      Subs :=
        Numeric.Integers.Range_Slice (First => 1, Last => 10) &
        Subscribe_Checker (Name     => "range_slice",
                           Do_Count => True, Ok_Count => 10,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 10);

      Subs :=
        Numeric.Integers.Range_Count (First => 1, Count => 0) &
        Subscribe_Checker (Name     => "range_count empty",
                           Do_Count => True, Ok_Count => 0);

      Subs :=
        Numeric.Integers.Range_Slice (First => 1, Last => 0) &
        Subscribe_Checker (Name     => "range_slice empty",
                           Do_Count => True, Ok_Count => 0);

      Subs :=
        Numeric.Integers.Range_Slice (First => 5, Last => 8) &
        Subscribe_Checker (Name     => "range_slice from /1",
                           Do_Count => True, Ok_Count => 4,
                           Do_First => True, Ok_First => 5,
                           Do_Last  => True, Ok_Last  => 8);

      Subs :=
        Start (Start_With_42'Access) &
        Subscribe_Checker (Name     => "start",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 42,
                           Do_Last  => True, Ok_Last  => 42);

      Subs :=
        Timer (1, 0.1) &
        Subscribe_Checker (Name     => "timer w interval",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);
      Subs :=
        Timer (0.1) &
        Subscribe_Checker (Name     => "timer default value",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 0,
                           Do_Last  => True, Ok_Last  => 0);

      --  Must wait or otherwise next texts may push those events
      --  too much in the future (see imprudent use of Hold in computation thread)
      while Subs.Is_Subscribed loop
         delay 0.1;
      end loop;

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
         Subs := Ob & Subscribe_Checker (Name     => "count",
                                         Do_Count => True, Ok_Count => 1,
                                         Do_First => True, Ok_First => 4,
                                         Do_Last  => True, Ok_Last  => 4);
         --  Both should report the same count
         Subs := Ob & Subscribe_Checker (Name     => "count isolation",
                                         Do_Count => True, Ok_Count => 1,
                                         Do_First => True, Ok_First => 4,
                                         Do_Last  => True, Ok_Last  => 4);
      end;

      --  Do_On
      Subs :=
        Ints.Just (1)
        & Do_On (Next => Test_On_Next'Access)
        & Subscribe;
      pragma Assert (Flag_On_Next, "Test_On_Next wasn't called");

      -- Test limit
      Subs :=
        Ints.From ((1, 3, 2)) &
        Limit (2) &
        Subscribe_Checker (Name     => "limit limiting",
                           Do_Count => True, Ok_Count => 2,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Ints.From ((1, 2, 3)) &
        Limit (5) & -- Check proper completion when not enough
        Subscribe_Checker (Name     => "limit not limiting",
                           Do_Count => True, Ok_Count => 3,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      -- Filter test
      Subs := Ints.From ((2, 2, 1)) &
        Filter (Is_One'Access) &
        Subscribe_Checker (Name     => "filter",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      -- Repeats
      Subs :=
        Just (1) &
        Repeat (9) & -- Standard repeating
        Subscribe_Checker (Name     => "repeat",
                           Do_Count => True, Ok_Count => 10,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      Subs :=
        Just (1) &
        While_Do (Never'Access) & -- Trivial instant exit
        Subscribe_Checker (Name     => "while",
                           Do_Count => True, Ok_Count => 0);

      Subs :=
        Just (1) &
        Repeat (20) &
        Take (10) & -- Repeat with limit
        Subscribe_Checker (Name     => "repeat & take",
                           Do_Count => True, Ok_Count => 10);

      Subs :=
        Just (1) &
        Repeat_Forever &
        Take (10) & -- Repeat forever with limit
        Subscribe_Checker (Name     => "repeat_forever & take",
                           Do_Count => True, Ok_Count => 10);

      Subs :=
        Just (1) &
        Repeat (4) &
        Take (10) & -- Repeat with unused limit
        Subscribe_Checker (Name     => "repeat & take more",
                           Do_Count => True, Ok_Count => 5);

      Subs :=
        From ((1, 2, 3)) &
        Repeat_Until (Always'Access) & -- Trivial exit after first repeat
        IntChecker.Subscribe (Name     => "repeat_until",
                              Do_Count => True, Ok_Count => 3);

      Subs :=
        Ints.From ((1, 2, 3)) &
        Repeat_Until (Actions.Count (Times => 3)) & -- Trivial exit after first repeat
        Subscribe_Checker (Name     => "repeat_until w action",
                           Do_Count => True, Ok_Count => 9,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Ints.From ((1, 2, 3)) &
        While_Do (not Actions.Count (Times => 3)) & -- Trivial exit after first repeat
        Subscribe_Checker (Name     => "while_do w action",
                           Do_Count => True, Ok_Count => 6,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 3);

      --  Casts
      Subs := Just (1.0) & Casts.To_Integer & IntChecker.Subscribe (Name => "cast float to int",  Do_Last => True, Ok_Last => 1);
      Subs := Just (1)   & Casts.To_Float   & FltChecker.Subscribe (Name => "cast int to float",  Do_Last => True, Ok_Last => 1.0);
      Subs := Just (1)   & Casts.To_String  & StrChecker.Subscribe (Name => "cast int to string", Do_Last => True, Ok_Last => "1");

      --  Last
      Subs :=
        From ((1, 2, 3)) &
        Last &
        Subscribe_Checker (Name     => "last",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 3,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        Integers.Empty &
        Last_Or_Default (3) &
        Subscribe_Checker (Name     => "last_or_default",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 3,
                           Do_Last  => True, Ok_Last  => 3);

      Subs :=
        From ((1, 2, 3)) &
        Last (Is_Even'Access) &
        Subscribe_Checker (Name     => "last conditional",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 2,
                           Do_Last  => True, Ok_Last  => 2);

      Subs :=
        From ((1, 3)) &
        Last_Or_Default (2, Is_Even'Access) &
        Subscribe_Checker (Name     => "last_or_default w condition",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 2,
                           Do_Last  => True, Ok_Last  => 2);

      Subs :=
        From ((1, 4)) &
        Last_Or_Default (2, Is_Even'Access) &
        Subscribe_Checker (Name     => "last_or_default w condition II",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 4,
                           Do_Last  => True, Ok_Last  => 4);

      -- Merge
      Subs :=
        Ints.From ((1, 2, 3))
        & Ints.Merge_With (From ((4, 5, 6)))
        & Subscribe_Checker (Name     => "merge-with",
                             Do_Count => True, Ok_Count => 6);

      Subs :=
        From ((1, 2, 3))
        & Merge_With (From ((4, 5, 6))
                      & Observe_On (Schedulers.Computation))
        & Numeric.Integers.Count
        & Subscribe_Checker (Name     => "merge-with & count w scheduler",
                             Do_Count => True, Ok_Count => 1,
                             Do_Last  => True, Ok_Last  => 6);

      Subs :=
        From ((1, 2, 3))
        & Observe_On (Schedulers.Immediate)
        & Ints.Merge_With (From ((4, 5, 6)))
        & Subscribe_Checker (Name     => "two-way merge, explicit",
                             Do_Count => True, Ok_Count => 6);

      Subs := -- This is implemented with as previous one internally
        Ints.Merge (From ((1, 2, 3)), From ((4, 5, 6)))
        & Subscribe_Checker (Name     => "two-way merge, implicit",
                             Do_Count => True, Ok_Count => 6);

      --------------
      -- Flat_Map --
      --------------

      Subs :=
        Ints.Empty
        & Ints.Flat_Map (Ints.No_Op)
        & Subscribe_Checker (Name     => "flatmap empty master",
                             Do_Count => True, Ok_Count => 0);

      Subs :=
        Ints.From ((1, 2, 3))
        & Ints.Flat_Map (Limit (0))
        & Subscribe_Checker (Name     => "flatmap empty subs pipeline",
                             Do_Count => True, Ok_Count => 0);

      Subs :=
        Ints.From ((1, 2, 3))
        & Ints.Flat_Map (Swallow'Access)
        & Subscribe_Checker (Name     => "flatmap empty subs inflater",
                             Do_Count => True, Ok_Count => 0);

      Subs :=
        Ints.From ((1, 2, 3))
        & Integer_To_String.Flat_Map (Ints.No_Op
                                      & Std.Casts.To_String
                                      & Strings.No_Op)
        & Subscribe_Checker (Name     => "flatmap AA-AB-BB pipe",
                             Do_First => True, Ok_First => "1",
                             Do_Last  => True, Ok_Last  => "3",
                             Do_Count => True, Ok_Count => 3);

      Subs :=
        Ints.From ((1, 2, 3))
        & Swallow'Access
        & Subscribe_Checker (Name     => "flatmap empty implicit",
                             Do_Count => True, Ok_Count => 0);

      -----------------
      -- Range_Slice --
      -----------------

      Subs :=
        Std.Numeric.Integers.Range_Slice (1, 4)
        & Ints.Flat_Map (Int_Emitter)
        & Subscribe_Checker (Name     => "flatmap immediate",
                             Do_Count => True, Ok_Count => 10);

      Subs :=
        Std.Numeric.Integers.Range_Slice (1, 5)
        & Ints.Flat_Map (Repeat (4)
                         & Observe_On (Schedulers.Computation)
                         & Hold (Fixed => 0.0, Random => 0.01))
        & Subscribe_Checker (Name     => "flatmap w pipeline, interleaving & scheduler",
                             Do_Count => True, Ok_Count => 25,
                             Period   => 2.0);
      while Subs.Is_Subscribed loop
         --  Wait for Schedulers.Computation to empty
         delay 0.1;
      end loop;

      Subs :=
        From ((1, 2, 3, 4, 5))
        & Integer_To_String.Flat_Map (AAA'Access)     -- <-- Flat_Map from Inflater
        & Flat_Map (Observe_On (Schedulers.Immediate) -- <-- Flat_Map from Pipeline
                    & Std.String_Succ'Access -- <-- implicit map
                    & No_Op)
        & Subscribe_Checker (Name     => "flatmap AB inflater & pipeline",
                             Do_Count => True, Ok_Count => 5,
                             Do_First => True, Ok_First => "b",
                             Do_Last  => True, Ok_Last  => "aaaab");

      ------------
      -- Expand --
      ------------

      Subs :=
        Just (1)
        & Expand (Selfsum'Access)
        & Limit (16)
        & Subscribe_Checker (Name     => "expand & limit",
                             Do_Count => True, Ok_Count => 16,
                             Do_First => True, Ok_First => 1,
                             Do_Last  => True, Ok_Last  => 32768);

      Subs :=
        From ((1, 2, 3, 4))
        & Expand (Below'Access)
        & Subscribe_Checker (Name     => "expand finite",
                             Do_Count => True, Ok_Count => 15,
                             Do_First => True, Ok_First => 1,
                             Do_Last  => True, Ok_Last  => 1);

      -- No_Op
      Subs :=
        Just (1) &
        No_Op &
        Subscribe_Checker (Name     => "no_op",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 1);

      -- Buffering
      Subs :=
        Numeric.Integers.Range_Count (1, 101) &
        Buffer (10) &
--          Std.Int_Images.Print &
        Numeric.Integers.Count &
--          Std.Int_Images.Print &
        Subscribe_Checker (Name     => "buffer",
                           Do_Count => True, Ok_Count => 1,
                           Do_First => True, Ok_First => 11,
                           Do_Last  => True, Ok_Last  => 11);

      -- Counting list sizes
      Subs :=
        Numeric.Integers.Range_Count (1, 11) &
        Buffer (10) &
--          Std.Int_Images.Print &
        Numeric.Integers.Length &
--          Std.Int_Images.Print &
        Subscribe_Checker (Name     => "buffer & length",
                           Do_Count => True, Ok_Count => 2,
                           Do_First => True, Ok_First => 10,
                           Do_Last  => True, Ok_Last  => 1);

      -- Splitting lists into elements
      Subs :=
        Numeric.Integers.Range_Count (1, 101) &
        Buffer (10) &
        Split &
        Buffer (7) &
        Split &
        Subscribe_Checker (Name     => "split",
                           Do_Count => True, Ok_Count => 101,
                           Do_First => True, Ok_First => 1,
                           Do_Last  => True, Ok_Last  => 101);

      -- Debouncing
      Subs :=
        Numeric.Integers.Range_Slice (1, 3)
        & Debounce (0.2) -- Should let pass only the last one
        & Subscribe_Checker (Name     => "debounce",
                             Do_Count => True, Ok_Count => 1,
                             Do_First => True, Ok_First => 3,
                             Do_Last  => True, Ok_Last  => 3);

      declare -- More precise debounce
         procedure Debounced (Observer : in out Std.Integers.Typed.Observer) is
         begin
            Observer.On_Next (1); delay 0.2;
            Observer.On_Next (2); delay 0.2;
            Observer.On_Next (3);
            Observer.On_Next (4);
            Observer.On_Complete ;
         end Debounced;
      begin
         For_Each (Integers.Create (Debounced'Access)
                   & Debounce (0.1),
                   Subscribe_Checker (Name     => "debounce w timings",
                     Do_First => True,  Ok_First => 1,
                                      Do_Last  => True,  Ok_Last  => 4,
                                      Do_Count => True,  Ok_Count => 3));
      end;

      -- Observe_On
      Subs :=
        From ((1, 2, 3))
        & Ints.Observe_On (Schedulers.Immediate)
        & Subscribe_Checker (Name     => "observer_on immediate",
                             Do_Count => True, Ok_Count => 3);

      Subs :=
        From ((1, 2, 3))
        & Ints.Observe_On (Schedulers.Computation)
        & Subscribe_Checker (Name     => "observe_on computation",
                             Do_Count => True, Ok_Count => 3);

      --  This checks a bug detected perviously in Examples.Threading
      Subs :=
        Ints.From ((1, 2, 3, 4, 5, 6))
        & Limit (5)
        & Observe_On (Schedulers.To_Scheduler (Custom_Next'Unrestricted_Access))
        & Observe_On (Schedulers.To_Scheduler (Custom_Next'Unrestricted_Access))
        & Observe_On (Schedulers.To_Scheduler (Custom_Idle'Unrestricted_Access))
        & Subscribe_Checker (Name     => "custom pool",
                             Do_Count => True, Ok_Count => 5);

      Subs :=
        Ints.Just (1)
        & Hold (Fixed => 0.5, Random => 0.0)
        & Stopwatch (Stopwatch_Test'Unrestricted_Access)
        & Subscribe_Checker (Name     => "hold & stopwatch",
                             Do_Count => True, Ok_Count => 1);

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
