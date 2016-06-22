with Ada.Exceptions;

	with Rx.Any;
	with Rx.Empty;
	with Rx.Errors;
	with Rx.Integers;
private with Rx.Interval;
	with Rx.Schedulers;

package Rx.Std is

--  Instances and default visibility for the common predefined types:
--  Strings, Integers, StrToInt, IntToInt, IntToStr
--  Also default sources/operators from ReactiveX documentation

   package Any renames Rx.Any.Instance;

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

   package RxEmpty is new Rx.Empty (Any.Typed);

   function Empty return Any.Observable renames RxEmpty.Empty;

   function Error (E : Rx.Errors.Occurrence)                return Any.Observable renames RxEmpty.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Any.Observable renames RxEmpty.Error;

   package RxInterval is new Rx.Interval (Integers.Typed, Integer'Succ);

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable renames RxInterval.Create;

   function Never return Any.Observable renames RxEmpty.Never;


end Rx.Std;
