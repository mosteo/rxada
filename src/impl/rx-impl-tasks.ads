private with Ada.Finalization;

package Rx.Impl.Tasks with Elaborate_Body is

   --  Root task interface for short-lived tasks
   --  Tasks of this kind can be reaped more easily

   type Transient is task interface;

   type Transient_Ptr is access all Transient'Class;

   procedure Reap_Now (This : in out Transient_Ptr);
   --  Will block until the task has Terminated

   type Reaper (Victim : not null Transient_Ptr) is limited private;
   --  This type must be declared at the task outer scope.
   --  Upon task termination will claim the task memory

private

   type Reaper (Victim : not null Transient_Ptr) is new Ada.Finalization.Limited_Controlled
   with null record;

   overriding procedure Finalize (This : in out Reaper);

end Rx.Impl.Tasks;
