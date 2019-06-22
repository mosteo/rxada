private with Ada.Containers.Ordered_Multisets;
private with Rx.Tools.Holders;
private with System.Address_Image;

package Rx.Dispatchers.Single is

   pragma Elaborate_Body;

   type Dispatcher is limited new Dispatchers.Dispatcher with private;

   type Ptr is access Dispatcher;

   --  Schedule a code to be run at a certain point from now, in a certain Dispatcher (thread)
   overriding
   procedure Schedule (Where : in out Dispatcher;
                       What  : Runnable'Class;
                       Time  : Ada.Calendar.Time := Ada.Calendar.Clock);

   not overriding
   function Is_Idle (This : in out Dispatcher) return Boolean;
   --  True if it is not running (but may have queued jobs for the future)

private

   -- This type is composed by a queue of events to run and a task that gets the first one and runs it.
   -- To Allow Termination, There'S A Notification System Between The Queue and The task.
   -- The Queue is Wrapped in A protected type.

   use Ada.Containers;
   use type Ada.Calendar.Time;

   type Dispatcher_Access is access all Dispatcher;

   package Runnable_Holders is new Rx.Tools.Holders (Runnable'Class, "single.runnable'class");
   subtype Runnable_Def is Runnable_Holders.Definite;

   type Event_Id is new Long_Long_Integer;

   type Event is record -- Needed To Hold It in The Ordered_Multiset
      Id   : Event_Id; -- Used to break time ties
      Time : Ada.Calendar.Time;
      Code : Runnable_Def;
   end record;

   function "<" (L, R : Event) return Boolean is
     (L.Time < R.Time or else (L.Time = R.Time and then L.Id < R.Id));

   package Event_Queues is new Ordered_Multisets (Event);

   --  To avoid any remaining possibility of race condition, and simultaneously
   --  allow automatic termination, we use a double-thread solution.
   --  It is heavier on the number of threads, but has all the desired
   --    advantages.
   --  The Queuer (or frontend) task is always accepting new events. While it
   --    has pending events, it tries continuously to pass one of these to the
   --    Runner (or backend) task, which is the one effectively taking care of
   --    the job.
   --  Thus, no block can happen since a Runner can always call its own Queuer

   task type Queuer (Parent : access Dispatcher) is
      entry Enqueue (R : Runnable'Class; Time : Ada.Calendar.Time);
      entry Reap; -- Used by Runner to notify runnable completion
   end Queuer;

   task type Runner (Parent : access Dispatcher) is
      entry Run (R : Runnable_Def);
   end Runner;

   function Addr_Img (This : in out Dispatcher) return String is
     ("#" & System.Address_Image (This'Address));

   type Dispatcher is limited new Dispatchers.Dispatcher with record
      Idle    : aliased Boolean := True with Atomic;
      Length  : aliased Natural := 0    with Atomic;
      Queue   : Queuer (Dispatcher'Access);
      Thread  : Runner (Dispatcher'Access);
   end record;

end Rx.Dispatchers.Single;
