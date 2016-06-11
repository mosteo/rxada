with Ada.Containers.Doubly_Linked_Lists;
with Ada.Real_Time.Timing_Events;

package Rx.Scheduler.Monocore is

   pragma Elaborate_Body;

   type Object is limited new Scheduler.Object with private;

   type Ptr is access Object;

   --  Schedule a code to be run at a certain point from now, in a certain scheduler (thread)
   overriding
   procedure Schedule (Where : in out Object; What : in out Runnable'Class; After : Duration := 0.0);

private


   package TE renames Ada.Real_Time.Timing_Events;

   type Object_Access is access all Object'Class;

   task type Runner (Parent : access Object'Class);

   use Ada.Containers;

   type Runnable_Access is access all Runnable'Class;

   package Runnable_Queues is new Doubly_Linked_Lists (Runnable_Access);

   protected type Safe (Parent : access Object'Class) is
      procedure OnEvent (Event : in out TE.Timing_Event);
      entry Get (R : out Runnable_Access);
   private
      Queue : Runnable_Queues.List;
   end Safe;

   type Object is limited new Scheduler.Object with record
      Thread  : Runner (Object'Access);
      Queue   : Safe (Object'Access);
   end record;

end Rx.Scheduler.Monocore;
