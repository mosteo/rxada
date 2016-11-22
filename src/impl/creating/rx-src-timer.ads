with Rx.Schedulers;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>); -- Items emitted
package Rx.Src.Timer with Elaborate_Body is

   function Create (V         : Typed.T;
                    Pause     : Duration;
                    Scheduler : Schedulers.Scheduler := Schedulers.Computation)
                    return Typed.Observable;

end Rx.Src.Timer;
