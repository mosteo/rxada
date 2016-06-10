with Rx.Schedulers;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observe_On is

   function Create (Scheduler : Schedulers.Scheduler) return Typed.Mutator'Class;

end Rx.Observe_On;
