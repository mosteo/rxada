private with Ada.Finalization;

package Rx.Impl.Tasks with Elaborate_Body is

   --  Root task interface for short-lived tasks
   --  Tasks of this kind can be reaped more easily

   type Transient     is task interface;
   type Transient_Ptr is access all Transient'Class;

   type Reaper (Victim : Transient_Ptr) is limited private;
   --  This type must be declared at the task outer scope to ensure Initialization
   --  Task memory will be reclaimed upon termination by GNAT runtime

private

   type Reaper (Victim : Transient_Ptr) is new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (This : in out Reaper);

end Rx.Impl.Tasks;
