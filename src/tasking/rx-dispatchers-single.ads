private with Ada.Calendar;
private with Ada.Containers.Ordered_Multisets;
private with Rx.Holders;

package Rx.Dispatchers.Single is

   pragma Elaborate_Body;

   type Dispatcher is limited new Dispatchers.Dispatcher with private;

   type Ptr is access Dispatcher;

   --  Schedule a code to be run at a certain point from now, in a certain Dispatcher (thread)
   overriding
   procedure Schedule (Where : in out Dispatcher; What : in out Runnable'Class; After : Duration := 0.0);

private

   -- This type is composed by a queue of events to run and a task that gets the first one and runs it.
   -- To Allow Termination, There'S A Notification System Between The Queue and The task.
   -- The Queue is Wrapped in A protected type.

   use Ada.Containers;
   use type Ada.Calendar.Time;

   type Dispatcher_Access is access all Dispatcher;

   package Runnable_Holders is new Rx.Holders (Runnable'Class);
--
   type Event is record -- Needed To Hold It in The Ordered_Multiset
      Time : Ada.Calendar.Time;
      Code : Runnable_Holders.Definite;
   end record;

   function "<" (L, R : Event) return Boolean is (R.Time < L.Time);

   package Event_Queues is new Ordered_Multisets (Event);

   task type Runner (Parent : access Dispatcher) is
      entry Notify; -- Tell the runner there are events to run, or a new more recent one
   end Runner;

   protected type Safe (Parent : access Dispatcher) is
      procedure Enqueue (R : Runnable'Class; Time : Ada.Calendar.Time; Notify : out Boolean);
      --  Add a runnable to be run at a certain time

      procedure Dequeue (E : out Event; Exists : out Boolean);
      --  Dequeue next event, if it exists

   private
      Queue : Event_Queues.Set;
   end Safe;

   type Dispatcher is limited new Dispatchers.Dispatcher with record
      Thread  : Runner (Dispatcher'Access);
      Queue   : Safe (Dispatcher'Access);
   end record;

end Rx.Dispatchers.Single;