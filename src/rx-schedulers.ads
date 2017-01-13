private with Ada.Task_Identification;

with Rx.Dispatchers;

private with Rx.Dispatchers.Immediate;
private with Rx.Dispatchers.Pools;

private with System.Multiprocessors;

package Rx.Schedulers is

   --  Note: there's is never, in the current implementation, reclaiming of created threads.
   --  So you might try to be conservative (i.e. use Idle_Thread instead of New_Thread).
   --  New_Thread may be justified in setup stages where you are reserving threads for tasks.

   --  For custom allocation of threads, see Schedulers.Pools

   type Scheduler is access all Rx.Dispatchers.Dispatcher'Class;

   function IO 		return Scheduler;
   --  This is backed by a thread pool that always returns an idle thread
   --  so it can grow unboundedly

   function Computation return Scheduler;
   --  This is backed by a thread pool with as many threads as CPUs

   function Idle_Thread return Scheduler;
   --  This is backed by a thread pool that is reused for idle threads whenever possible
   --  Like IO, can grow as much as needed

   function New_Thread  return Scheduler;
   --  Returns a new thread that won't be used anywhere else (nor garbage collected!)

   function Immediate 	return Scheduler;
   --  Does nothing, code is executed as it arrives

   procedure Shutdown renames Dispatchers.Shutdown;
   --  Signal schedulers to exit.
   --  Necessary when there are infinite sequences going on (e.g. Interval)

   function Terminating return Boolean renames Dispatchers.Terminating;
   --  Will be true after shutdown has been invoked

   function Current_Thread_Id return String;
   --  Shortcut for Ada.Task_Identification

private

   use Rx.Dispatchers;


   Pool_CPU  : Pools.Pool (Positive (System.Multiprocessors.Number_Of_CPUs), new String'("CPU"));
   Pool_IO   : Pools.Pool (1, new String'("IO"));
   Pool_Idle : Pools.Pool (1, new String'("Idle"));
   Pool_Excl : Pools.Pool (1, new String'("Excl"));

   Real_Immed : aliased Dispatchers.Immediate.Dispatcher;
   function Immediate return Scheduler is (Real_Immed'Access);

   function IO return Scheduler is (Scheduler (Pool_IO.Find_Idle));

   function Computation return Scheduler is (Scheduler (Pool_CPU.Get));

   function Idle_Thread return Scheduler is (Scheduler (Pool_Idle.Find_Idle));

   function New_Thread return Scheduler is (Scheduler (Pool_Excl.Grow));

   use Ada.Task_Identification;
   function Current_Thread_Id return String is (Image (Current_Task));

end Rx.Schedulers;
