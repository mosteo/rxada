with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Observe_On is

   package Remote is new Dispatchers.Events (Operate.Typed);
   package Shared renames Remote.Shared;

   type Op is new Operate.Transform.Operator with record
      Sched : Schedulers.Scheduler;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T);
   overriding procedure On_Completed (This : in out Op);
   overriding procedure On_Error     (This : in out Op; Error : in out Rx.Errors.Occurrence);

   overriding procedure Subscribe    (This : in out Op; Child : in out Operate.Observer);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
   begin
      Remote.On_Next (This.Sched.all, Shared.Observer (This.Get_Child.Actual.all), V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Op) is
   begin
      Remote.On_Completed (This.Sched.all, Shared.Observer (This.Get_Child.Actual.all));
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : in out Rx.Errors.Occurrence) is
   begin
      Remote.On_Error (This.Sched.all, Shared.Observer (This.Get_Child.Actual.all), Error);
      --  Since the error is now in another thread, and we won't know if it has been handled,
      --  we are done here:
      Error.Set_Handled;
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Child : in out Operate.Observer) is
      Parent : Operate.Observable := This.Get_Parent;
      Me     : Op := This; -- Create a copy that will hold the actual shared observable
   begin
      Me.Set_Child (Shared.Create (Child));
      Parent.Subscribe (Me);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator is
   begin
      return Op'(Operate.Transform.Operator with Sched => Scheduler);
   end Create;

end Rx.Op.Observe_On;
