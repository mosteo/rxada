private with Ada.Task_Identification;

private with Rx.Lazy;
with Rx.Scheduler.Immediate;
with Rx.Scheduler.Monocore;

package Rx.Schedulers is

   Type Scheduler is access all Rx.Scheduler.Object'Class;

   function IO 		return Scheduler;
   function Background 	return Scheduler;
   function Computation return Scheduler;
   function Immediate 	return Scheduler;
   function New_Thread  return Scheduler;

   --  Shortcut for Ada.Task_Identification
   function Current_Thread_Id return String;

private

   use Rx.Scheduler;

   type Lazy_Single is new Rx.Lazy (Rx.Scheduler.Monocore.Object, Rx.Scheduler.Monocore.Ptr);

   Real_Immed  : aliased Rx.Scheduler.Immediate.Object;
   function Immediate return Scheduler is (Real_Immed'Access);

   use Ada.Task_Identification;
   function Current_Thread_Id return String is (Image (Current_Task));

end Rx.Schedulers;
