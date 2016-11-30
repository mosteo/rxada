with Rx.Observables;
with Rx.Schedulers;

generic
   with package Observables is new Rx.Observables (<>);
   with function To_Numeric (I : Long_Long_Integer) return Observables.T;
   with function Succ (V : Observables.T) return Observables.T is <>;
   with function "<"  (L, R : Observables.T) return Boolean is <>;
package Rx.Numeric_Observables is

   subtype Observable is Observables.Observable;
   subtype T          is Observables.T;

   function Interval (First       : T;
                      Pause       : Duration             := 1.0;
                      First_Pause : Duration             := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Observable;

   function Range_Count (First : T;
                         Count : Rx_Natural) return Observable;

   function Range_Slice (First : T;
                         Last  : T) return Observable;

end Rx.Numeric_Observables;
