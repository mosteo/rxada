with Rx.Operate;
with Rx.Schedulers;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.Op.Subscribe_On is

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class;

end Rx.Op.Subscribe_On;
