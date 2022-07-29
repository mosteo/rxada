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

   type Thread is access all Rx.Dispatchers.Dispatcher'Class;
   --  An actual scheduler thread

   type Pool is limited interface;

   type Pool_Access is access all Pool'Class;

   function Get_Thread (This : in out Pool) return Thread is abstract;

   type Thread_Allocator is access function return Thread;

   -----------------
   --  Scheduler  --
   -----------------

   type Scheduler is tagged private;

   function Get_Thread (This : Scheduler) return Thread;
   --  This retrieves one thread reference for use

   function To_Scheduler (This : Thread_Allocator)   return Scheduler;
   function To_Scheduler (This : aliased in out Pool'Class) return Scheduler;
   --  Two ways of obtaining a Scheduler

   --  Operators that change thread (Observe_On/Subscribe_On) do so only once,
   --    during Subscribe. Hence these operators must store a scheduler

   --------------------------
   --  Default Schedulers  --
   --------------------------

   function Input_Output return Scheduler;
   function IO 		 return Scheduler renames Input_Output; -- Rx usual name
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

   type Scheduler is tagged record
      Allocator : Thread_Allocator;
      Pool      : Pool_Access;
   end record;

   function To_Scheduler (This : Thread_Allocator)   return Scheduler is
     (Scheduler'(Allocator => This,
                 Pool      => null));

   function To_Scheduler (This : aliased in out Pool'Class) return Scheduler is
     (Scheduler'(Allocator => null,
                 Pool      => This'Unchecked_Access));

   function Get_Thread (This : Scheduler) return Thread is
     (if This.Allocator /= null then This.Allocator.all
      elsif This.Pool   /= null then This.Pool.Get_Thread
      else raise Program_Error with "Uninitialized Scheduler");

   use Rx.Dispatchers;

   Pool_CPU  : Pools.Pool (Positive (System.Multiprocessors.Number_Of_CPUs), new String'("CPU"));
   Pool_IO   : Pools.Pool (1, new String'("IO"));
   Pool_Idle : Pools.Pool (1, new String'("Idle"));
   Pool_Excl : Pools.Pool (1, new String'("Excl"));

   Real_Immed : aliased Dispatchers.Immediate.Dispatcher;
   function Immediate_Impl return Thread    is (Real_Immed'Access);
   function Immediate      return Scheduler is (To_Scheduler (Immediate_Impl'Access));

   function IO_Impl return Thread    is (Thread (Pool_IO.Find_Idle));
   function Input_Output return Scheduler is (To_Scheduler (IO_Impl'Access));

   function Computation_Impl return Thread    is (Thread (Pool_CPU.Get));
   function Computation      return Scheduler is (To_Scheduler (Computation_Impl'Access));

   function Idle_Thread_Impl return Thread    is (Thread (Pool_Idle.Find_Idle));
   function Idle_Thread      return Scheduler is (To_Scheduler (Idle_Thread_Impl'Access));

   function New_Thread_Impl return Thread    is (Thread (Pool_Excl.New_One));
   function New_Thread      return Scheduler is (To_Scheduler (New_Thread_Impl'Access));

   use Ada.Task_Identification;
   function Current_Thread_Id return String is (Image (Current_Task));

end Rx.Schedulers;
