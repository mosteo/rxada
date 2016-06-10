private with Ada.Task_Identification;

with Rx.Scheduler.Immediate;
with Rx.Scheduler.Monocore;

package Rx.Schedulers is

   Type Scheduler is access all Rx.Scheduler.Object'Class;

   IO          : constant Scheduler;
   Background  : constant Scheduler;
   Computation : constant Scheduler;
   Immediate   : constant Scheduler;

   --  Still missing: NewThread, trampoline (yuks!)

   --  Shortcut for Ada.Task_Identification
   function Current_Task_Id return String;

private

   use Rx.Scheduler;

   Real_IO     : aliased Monocore.Object;
   IO          : constant Scheduler := Real_IO'Access;

   Real_BG     : aliased Monocore.Object;
   Background  : constant Scheduler := Real_BG'Access;

   Real_Comp   : aliased Monocore.Object;
   Computation : constant Scheduler := Real_Comp'Access;

   Real_Immed  : aliased Rx.Scheduler.Immediate.Object;
   Immediate   : constant Scheduler := Real_Immed'Access;

   use Ada.Task_Identification;
   function Current_Task_Id return String is (Image (Current_Task));

end Rx.Schedulers;
