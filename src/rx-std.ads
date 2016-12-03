with Ada.Exceptions;

with Rx.Errors;
with Rx.Impl.Casts;
with Rx.Impl.Std;
with Rx.Observables.Image;
with Rx.Operators;
with Rx.Subscriptions;
with Rx.Schedulers;

private with Rx.Src.Empty;
private with Rx.Src.Interval;
private with Rx.Src.Timer;

package Rx.Std is

   --  Instances and default visibility for some predefined types
   --  Also default sources/operators from ReactiveX documentation

   --  Note that default integers are not Standard.Integer but the largest integer type supported

   --  Type shortcuts:

   subtype Subscription is Rx.Subscriptions.Subscription;

   --  Convenience instances

   --  Base Std types

   package Any      renames Impl.Std.Any.Instance.Observables;
   package Integers renames Impl.Std.Integers.Observables;
   package Floats   renames Impl.Std.Floats.Observables;
   package Strings  renames Impl.Std.Strings.Observables;

   --  Numeric self-operations for base types

   package Numeric renames Impl.Std.Numeric;

   --  Transforming operators between base types

   package AnyToFlt renames Impl.Std.Any_To_Float;
   package IntToFlt renames Impl.Std.Int_To_Float;
   package StrToFlt renames Impl.Std.StrToFlt;

   package AnyToInt renames Impl.Std.AnyToInt;
   package FltToInt renames Impl.Std.FltToInt;
   package StrToInt renames Impl.Std.StrToInt;

   package AnyToStr renames Impl.Std.AnyToStr;
   package FltToStr renames Impl.Std.FltToStr;
   package IntToStr renames Impl.Std.IntToStr;

   function String_Succ (S : String) return String;
   -- Lexicographic enumeration over the Character type. Useless I guess.

   --  Standard Rx sources and operators

   package Casts is

      --  Casts for predefined types

      function To_Float return IntToFlt.Operator is (IntToFlt.Map (Rx.Impl.Casts.To_Float'Access));
      function To_Float return StrToFlt.Operator is (StrToFlt.Map (Rx.Impl.Casts.To_Float'Access));

      function To_Integer return FltToInt.Operator is (FltToInt.Map (Rx.Impl.Casts.To_Integer'Access));
      function To_Integer return StrToInt.Operator is (StrToInt.Map (Rx.Impl.Casts.To_Integer'Access));

      function To_String return FltToStr.Operator is (FltToStr.Map (Rx.Impl.Casts.To_String'Access));
      function To_String return IntToStr.Operator is (IntToStr.Map (Rx.Impl.Casts.To_String'Access));

   end Casts;

   function Empty return Any.Observable;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable;

   package Images is

      package Floats   is new Std.Floats.Image   (Impl.Casts.To_String);
      package Integers is new Std.Integers.Image (Impl.Casts.To_String);

   end Images;

   function Interval (First       : Rx_Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable;

   function Never return Any.Observable;

   function Timer (After : Duration) return Integers.Observable;
   --  Std Timer emits a 0 after Pause seconds an completes

private

   package RxEmpty    is new Rx.Src.Empty    (Any.Typed);
   package RxInterval is new Rx.Src.Interval (Integers.Typed, Rx_Integer'Succ);
   package RxTimer    is new Rx.Src.Timer    (Integers.Typed);

   function Empty return Any.Observable renames RxEmpty.Empty;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable renames RxEmpty.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable renames RxEmpty.Error;

   function Interval (First       : Rx_Integer := 0;
                      Pause       : Duration   := 1.0;
                      First_Pause : Duration   := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable renames RxInterval.Create;

   function Never return Any.Observable renames RxEmpty.Never;

   function String_Succ (S : String) return String
   is (if    S'Length = 0                 then String'(1 => Character'First)
       elsif S (S'Last) /= Character'Last then S (S'First .. S'Last - 1) & Character'Succ (S (S'Last))
       else  S & Character'First);

   function Timer (After : Duration) return Integers.Observable is
      (RxTimer.Create (0, After));

end Rx.Std;
