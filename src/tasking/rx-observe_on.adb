with Rx.Errors;
with Rx.Scheduler;

package body Rx.Observe_On is

   package Events is new Scheduler.Events (Operate.Typed);
   package Shared renames Events.Shared;

   type Op is new Operate.Transform.Operator with record
      Sched : Schedulers.Scheduler;
      Child : Shared.Observer; -- The regular child in Transform.Operator is not useful in this special case
   end record;

   pragma Compile_Time_Warning (True, "On_Error unimplemented");

   overriding procedure On_Next      (This : in out Op; Child : in out Operate.Observer; V : Operate.T);
   overriding procedure On_Completed (This : in out Op; Child : in out Operate.Observer);
   overriding procedure Subscribe    (This : in out Op; Child : in out Operate.Observer);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; Child : in out Operate.Observer; V : Operate.T) is
   begin
      Events.On_Next (This.Sched.all, This.Child, V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Op; Child : in out Operate.Observer) is
   begin
      Events.On_Completed (This.Sched.all, This.Child);
   end On_Completed;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Child : in out Operate.Observer) is
      Parent : Operate.Observable := This.Get_Parent;
      Me     : Op := This; -- Create a copy that will hold the actual shared observable
   begin
      Me.Child := Shared.Create (Child);
      Parent.Subscribe (Me);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator is
   begin
      return Op'(Operate.Transform.Operator with Sched => Scheduler, Child => Shared.Null_Observer);
   end Create;

end Rx.Observe_On;
