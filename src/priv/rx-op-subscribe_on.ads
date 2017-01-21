with Rx.Impl.Preservers;
with Rx.Schedulers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Subscribe_On is

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class;

end Rx.Op.Subscribe_On;
