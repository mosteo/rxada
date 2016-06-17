private with Ada.Task_Identification;

with Rx.Dispatchers;
private with Rx.Dispatchers.Immediate;
private with Rx.Dispatchers.Single;
private with Rx.Lazy;

package Rx.Schedulers is

   Type Scheduler is access all Rx.Dispatchers.Dispatcher'Class;

   function IO 		return Scheduler;
   function Background 	return Scheduler;
   function Computation return Scheduler;
   function Immediate 	return Scheduler;
--   function New_Thread  return Scheduler;

   procedure Shutdown renames Dispatchers.Shutdown;
   --  Signal schedulers to exit.
   --  Necessary when there are infinite sequences going on (e.g. Interval)

   function Terminating return Boolean renames Dispatchers.Terminating;
   --  Will be true after shutdown has been invoked

   function Current_Thread_Id return String;
   --  Shortcut for Ada.Task_Identification

private

   use Rx.Dispatchers;

   package Lazy_Single is new Rx.Lazy (Rx.Dispatchers.Single.Dispatcher, Rx.Dispatchers.Single.Ptr);

   Real_Immed : aliased Dispatchers.Immediate.Dispatcher;
   function Immediate return Scheduler is (Real_Immed'Access);

   Real_IO : Lazy_Single.Lazy;
   function IO return Scheduler is (Scheduler (Real_IO.Get));

   Real_Background : Lazy_Single.Lazy;
   function Background return Scheduler is (Scheduler (Real_Background.Get));

   Real_Computation : Lazy_Single.Lazy;
   function Computation return Scheduler is (Scheduler (Real_Computation.Get));

   use Ada.Task_Identification;
   function Current_Thread_Id return String is (Image (Current_Task));

end Rx.Schedulers;
