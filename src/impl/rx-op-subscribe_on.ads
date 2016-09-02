with Rx.Operate;
with Rx.Schedulers;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.Subscribe_On is

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator;

end Rx.Subscribe_On;
