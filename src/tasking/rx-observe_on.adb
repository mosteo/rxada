with Rx.Dispatchers;
with Rx.Errors;

package body Rx.Observe_On is

   package Events is new Dispatchers.Events (Operate.Typed);
   package Shared renames Events.Shared;

   type Op is new Operate.Transform.Operator with record
      Sched : Schedulers.Scheduler;
      Child : Shared.Observer; -- The regular child in Transform.Operator is not useful in this special case
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T);
   overriding procedure On_Completed (This : in out Op);
   overriding procedure On_Error     (This : in out Op; Error : Rx.Errors.Occurrence);

   --  Those shouldn't be called anyway
   overriding procedure On_Next      (This : in out Op; Child : in out Operate.Observer; V : Operate.T);
   overriding procedure On_Completed (This : in out Op; Child : in out Operate.Observer);

   overriding procedure Subscribe    (This : in out Op; Child : in out Operate.Observer);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next      (This : in out Op; V : Operate.T) is
   begin
      Events.On_Next (This.Sched.all, This.Child, V);
   end On_Next;

   overriding procedure On_Next (This : in out Op; Child : in out Operate.Observer; V : Operate.T) is
   begin
      This.On_Next (V); -- Just in case
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Op) is
   begin
      Events.On_Completed (This.Sched.all, This.Child);
   end On_Completed;

   overriding procedure On_Completed (This : in out Op; Child : in out Operate.Observer) is
   begin
      This.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : Rx.Errors.Occurrence) is
   begin
      Events.On_Error (This.Sched.all, This.Child, Error);
   end On_Error;

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
