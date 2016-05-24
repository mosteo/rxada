with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Real_Time.Timing_Events;

package Rx.Scheduler.Monocore is

   pragma Elaborate_Body;

   type Object is limited new Scheduler.Object with private;

   --  Schedule a code to be run at a certain point from now, in a certain scheduler (thread)
   overriding
   procedure Schedule (Where : in out Object; What : Runnable'Class; After : Duration := 0.0);

private


   package TE renames Ada.Real_Time.Timing_Events;

   type Object_Access is access all Object'Class;

   task type Runner (Parent : access Object'Class);

   use Ada.Containers;

   package Runnable_Holders is new Indefinite_Holders (Runnable'Class);
   type Runnable_Holder is new Runnable_Holders.Holder with null record;

   package Runnable_Queues is new Indefinite_Doubly_Linked_Lists (Runnable'Class);

   protected type Safe (Parent : access Object'Class) is
      procedure OnEvent (Event : in out TE.Timing_Event);
      entry Get (R : out Runnable_Holder);
   private
      Queue : Runnable_Queues.List;
   end Safe;

   type Object is limited new Scheduler.Object with record
      Thread  : Runner (Object'Access);
      Queue   : Safe (Object'Access);
   end record;

end Rx.Scheduler.Monocore;
