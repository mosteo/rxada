	with Rx.Integers;
private with Rx.Interval;
	with Rx.Schedulers;

package Rx.Standard is

--  Instances and default visibility for the common predefined types:
--  Strings, Integers, StrToInt, IntToInt, IntToStr
--  Also default sources/operators from ReactiveX documentation

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable;

private

   package RxInterval is new Rx.Interval (Integers.Typed, Integer'Succ);

   function Interval (First       : Integer := 0;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable renames RxInterval.Create;


end Rx.Standard;
