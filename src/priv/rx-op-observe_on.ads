with Rx.Impl.Preservers;
with Rx.Schedulers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Observe_On is

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class;

end Rx.Op.Observe_On;
