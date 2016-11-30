with Ada.Exceptions;

with Rx.Errors;
with Rx.Impl.Casts;
with Rx.Impl.Std;
--  with Rx.Numeric_Observables;
-- with Rx.Numeric_Operators;
with Rx.Observables.Image;
with Rx.Operators;
with Rx.Subscriptions;
with Rx.Schedulers;

private with Rx.Src.Empty;
private with Rx.Src.Interval;
private with Rx.Src.Timer;

package Rx.Std is

--  Instances and default visibility for the common predefined types:
--  Strings, Integers, StrToInt, IntToInt, IntToStr
--  Also default sources/operators from ReactiveX documentation

   --  Note that default integers are not Standard.Integer but the largest integer type supported

   --  Type shortcuts:

   subtype Subscription is Rx.Subscriptions.Subscription;

   --  Convenience instances

   package Any      renames Rx.Impl.Std.Any.Instance.Observables;
   package Integers renames Rx.Impl.Std.Integers.Observables;
   package Floats   renames Rx.Impl.Std.Floats.Observables;
   package Strings  renames Rx.Impl.Std.Strings.Observables;

   package AnyToFlt is new Rx.Operators (Any,      Floats);
   package IntToFlt is new Rx.Operators (Integers, Floats);
   package StrToFlt is new Rx.Operators (Strings,  Floats);

   package AnyToInt is new Rx.Operators (Any,     Integers);
   package FltToInt is new Rx.Operators (Floats,  Integers);
   package StrToInt is new Rx.Operators (Strings, Integers);

   package AnyToStr is new Rx.Operators (Any,      Strings);
   package FltToStr is new Rx.Operators (Floats,   Strings);
   package IntToStr is new Rx.Operators (Integers, Strings);

   package FltCount is new FltToInt.Counters (Integer'Succ, 0);
   package IntCount is new Integers.Counters (Integer'Succ, 0);
   package StrCount is new StrToInt.Counters (Integer'Succ, 0);

   function String_Succ (S : String) return String;
   -- Lexicographic enumeration over the Character type. Useless I guess.

   package IntEnums is new Integers.Enums (Integer'Succ);
   package StrEnums is new Strings.Enums  (String_Succ);

--   package IntImg is new Integers.Image (Integer'Image);

   --  Standard Rx sources and operators

   function Empty return Any.Observable;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable;

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable;

   function List_Length (L : Integers.T_List) return Integer is (Integer (L.Length));
   function Length is new Integers.Length (List_Length);

   function Never return Any.Observable;

   function Timer (After : Duration) return Integers.Observable;
   --  Std Timer emits a 0 after Pause seconds an completes

   --  Casts for predefined types

   Float_To_Integer  : constant FltToInt.Operator := FltToInt.Map (Rx.Impl.Casts.To_Integer'Access);
   Float_To_String   : constant FltToStr.Operator := FltToStr.Map (Rx.Impl.Casts.To_String'Access);

   Integer_To_Float  : constant IntToFlt.Operator := IntToFlt.Map (Rx.Impl.Casts.To_Float'Access);
   Integer_To_String : constant IntToStr.Operator := IntToStr.Map (Rx.Impl.Casts.To_String'Access);

   String_To_Float   : constant StrToFlt.Operator := StrToFlt.Map (Rx.Impl.Casts.To_Float'Access);
   String_To_Integer : constant StrToInt.Operator := StrToInt.Map (Rx.Impl.Casts.To_Integer'Access);

   --  Printing

   package Int_Images is new Integers.Image (Impl.Casts.To_String);

private

   package RxEmpty    is new Rx.Src.Empty    (Any.Typed);
   package RxInterval is new Rx.Src.Interval (Integers.Typed, Integer'Succ);
   package RxTimer    is new Rx.Src.Timer    (Integers.Typed);

   function Empty return Any.Observable renames RxEmpty.Empty;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable renames RxEmpty.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable renames RxEmpty.Error;

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
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
