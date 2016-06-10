with Rx.Errors;
with Rx.Scheduler;

package body Rx.Observe_On is

   package Events is new Scheduler.Events (Typed);
   package Shared renames Events.Shared;

   type Op is new Typed.Mutator with record
      Sched : Schedulers.Scheduler;
      Child : Shared.Observer;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Typed.Type_Traits.T);
   overriding procedure On_Completed (This : in out Op);
   overriding procedure On_Error     (This : in out Op; Error : Errors.Occurrence);
   overriding procedure Subscribe    (This : in out Op; Observer : in out Typed.Consumers.Observer'Class);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next      (This : in out Op; V : Typed.Type_Traits.T) is
   begin
      Events.On_Next (This.Sched.all, This.Child, V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Op) is
   begin
      Events.On_Completed (This.Sched.all, This.Child);
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error     (This : in out Op; Error : Errors.Occurrence) is
   begin
      Events.On_Error (This.Sched.all, This.Child, Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe    (This : in out Op; Observer : in out Typed.Consumers.Observer'Class) is
      Parent : Typed.Producers.Observable'Class := This.Get_Parent;
      Me     : Op := This; -- Create a copy that will hold the actual shared observable
   begin
      Me.Child := Shared.Create (Observer);
      Parent.Subscribe (Me);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Typed.Mutator'Class is
   begin
      return Op'(Typed.Mutator with Sched => Scheduler, Child => Shared.Null_Observer);
   end Create;

end Rx.Observe_On;
