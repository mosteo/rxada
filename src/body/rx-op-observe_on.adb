with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Observe_On is

   package Remote is new Dispatchers.Events (Operate.Typed);
   package Shared renames Remote.Shared;

   type Op is new Operate.Operator with record
      Scheduler  : Schedulers.Scheduler;
      Subscriber : Shared.Observer;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T);
   overriding procedure On_Complete  (This : in out Op);
   overriding procedure On_Error     (This : in out Op; Error : Errors.Occurrence);

   overriding procedure Subscribe    (This : in out Op; Observer : in out Operate.Into.Observer'Class);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
   begin
      Remote.On_Next (This.Scheduler.all, This.Subscriber, V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Op) is
   begin
      Remote.On_Complete  (This.Scheduler.all, This.Subscriber);
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : Errors.Occurrence) is
   begin
      Remote.On_Error (This.Scheduler.all, This.Subscriber, Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Observer : in out Operate.Into.Observer'Class) is
   begin
      This.Subscriber := Shared.Create (Observer);
      Operate.Operator (This).Subscribe (This.Subscriber);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class is
   begin
      return Op'(Operate.Operator with
                   Scheduler  => Scheduler,
                   Subscriber => <>); -- To be set during subscription
   end Create;

end Rx.Op.Observe_On;
