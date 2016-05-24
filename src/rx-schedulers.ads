with Rx.Scheduler;

package Rx.Schedulers is

   IO          : Rx.Scheduler.Object;
   Background  : Rx.Scheduler.Object;
   Computation : Rx.Scheduler.Object;

   --  Still missing: NewThread, immediate, trampoline (yuks!)

end Rx.Schedulers;
