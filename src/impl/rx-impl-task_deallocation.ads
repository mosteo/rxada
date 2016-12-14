private with Ada.Finalization;
private with Ada.Task_Identification;

generic
   type Task_Type (<>) is limited private;
   type Ptr is access Task_Type;
package Rx.Impl.Task_Deallocation with Elaborate_Body is

   --  This package is an attempt at workaround for problems with 'Terminated on task interfaces

   type Reaper (Victim : not null Ptr) is limited private;
   --  This type must be declared at the task outer scope.
   --  Upon task termination will claim the task memory

private

   type Reaper (Victim : not null Ptr) is new Ada.Finalization.Limited_Controlled
   with record
      Id : Ada.Task_Identification.Task_Id;
   end record;

   overriding procedure Initialize (This : in out Reaper);
   overriding procedure Finalize   (This : in out Reaper);

end Rx.Impl.Task_Deallocation;
