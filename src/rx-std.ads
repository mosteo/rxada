with Ada.Exceptions;

with Rx.Errors;
with Rx.Impl.Any;
with Rx.Impl.Integers;
with Rx.Impl.Strings;
with Rx.Operators;
with Rx.Subscriptions;
with Rx.Schedulers;

private with Rx.Src.Empty;
private with Rx.Src.Interval;

package Rx.Std is

--  Instances and default visibility for the common predefined types:
--  Strings, Integers, StrToInt, IntToInt, IntToStr
--  Also default sources/operators from ReactiveX documentation

   --  Type shortcuts:

   subtype Subscription is Rx.Subscriptions.Subscription;

   --  Convenience instances

   package Any      renames Rx.Impl.Any.Instance.Observables;
   package Integers renames Rx.Impl.Integers.Observables;
   package Strings  renames Rx.Impl.Strings.Observables;

   package StrToInt is new Rx.Operators (Strings, Integers);
   package IntToStr is new Rx.Operators (Integers, Strings);

   package IntCount is new Integers.Counters (Integer'Succ, 0);
   package StrCount is new StrToInt.Counters (Integer'Succ, 0);

   --  Standard Rx sources and operators

   function Empty return Any.Observable;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable;

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable;

   function Never return Any.Observable;

private

   package RxEmpty is new Rx.Src.Empty (Any.Typedd);

   function Empty return Any.Observable renames RxEmpty.Empty;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable renames RxEmpty.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable renames RxEmpty.Error;

   package RxInterval is new Rx.Src.Interval (Integers.Typedd, Integer'Succ);

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable renames RxInterval.Create;

   function Never return Any.Observable renames RxEmpty.Never;

end Rx.Std;
