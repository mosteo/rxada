with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Observe_On is

   package Remote is new Dispatchers.Events (Operate.Typed);
   package Shared renames Remote.Shared;

   type Op is new Operate.Preserver with record
      Shubs : Shared.Subscriber;
      Sched : Schedulers.Scheduler;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T; Child : in out Operate.Observer'Class);
   overriding procedure On_Completed (This : in out Op; Child : in out Operate.Observer'Class);
   overriding procedure On_Error     (This : in out Op; Error : in out Rx.Errors.Occurrence; Child : in out Operate.Observer'Class);

   overriding procedure Subscribe    (This : in out Op; Child : in out Operate.Subscriber);
   overriding procedure Unsubscribe  (This : in out Op);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T; Child : in out Operate.Observer'Class) is
   begin
      Remote.On_Next (This.Sched.all, Shared.Subscriber (Child), V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Op; Child : in out Operate.Observer'Class) is
   begin
      Remote.On_Completed (This.Sched.all, Shared.Subscriber (Child));
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : in out Rx.Errors.Occurrence; Child : in out Operate.Observer'Class) is
   begin
      Remote.On_Error (This.Sched.all, Shared.Subscriber (Child), Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Child : in out Operate.Subscriber) is
      Parent : Operate.Observable := This.Get_Parent;
      Me     : Op := This; -- Create a copy that will hold the actual shared observable
   begin
      Me.Shubs := Shared.Create (Child);
      Me.Set_Child (Me.Shubs); -- Stored twice to have it here for Unsubscribe
      Parent.Subscribe (Me);
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Op) is
   begin
      Remote.Unsubscribe (This.Sched.all, This.Shubs);
   end Unsubscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Preserver'Class is
   begin
      return Op'(Operate.Transform.Transformer with Sched => Scheduler, Shubs => <>);
   end Create;

end Rx.Op.Observe_On;
