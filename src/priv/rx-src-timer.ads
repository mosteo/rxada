with Rx.Schedulers;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>); -- Items emitted
package Rx.Src.Timer with Elaborate_Body is

   function Create (V         : Typed.T;
                    After     : Duration;
                    Scheduler : Schedulers.Scheduler := Schedulers.Computation)
                    return Typed.Observable;

end Rx.Src.Timer;
