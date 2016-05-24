with Rx.Scheduler.Monocore;
use Rx.Scheduler;

package Rx.Schedulers is

   Type Object is access all Rx.Scheduler.Object'Class;

   IO          : constant Object;
   Background  : constant Object;
   Computation : constant Object;

   --  Still missing: NewThread, immediate, trampoline (yuks!)

private

   Real_IO     : aliased Monocore.Object;
   IO          : constant Object := Real_IO'Access;

   Real_BG     : aliased Monocore.Object;
   Background  : constant Object := Real_BG'Access;

   Real_Comp   : aliased Monocore.Object;
   Computation : constant Object := Real_Comp'Access;

end Rx.Schedulers;
