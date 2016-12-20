with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Observe_On is

   package Remote is new Dispatchers.Events (Operate.Typed);
   package Shared renames Remote.Shared;

   type Op is new Operate.Implementation.Operator with record
      Downstream : Shared.Subscriber;
      Scheduler  : Schedulers.Scheduler;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T);
   overriding procedure On_Completed (This : in out Op);
   overriding procedure On_Error     (This : in out Op; Error : Errors.Occurrence);

   overriding procedure Subscribe    (This : in out Op; Observer : Operate.Into.Subscriber'Class);
   overriding procedure Unsubscribe  (This : in out Op);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
   begin
      Remote.On_Next (This.Scheduler.all, This.Downstream, V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Op) is
   begin
      Remote.On_Completed (This.Scheduler.all, This.Downstream);
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : Errors.Occurrence) is
   begin
      Remote.On_Error (This.Scheduler.all, This.Downstream, Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Observer : Operate.Into.Subscriber'Class) is
   begin
      This.Downstream := Shared.Create (Observer);
      Operate.Implementation.Operator (This).Subscribe (This.Downstream);
      --  Get_Subscriber not to be used directly, so this subscription could use an always failing observer
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Op) is
   begin
      Remote.Unsubscribe (This.Scheduler.all, This.Downstream);
   end Unsubscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class is
   begin
      return Operate.Create (Op'(Operate.Implementation.Operator with
                                 Scheduler  => Scheduler,
                                 Downstream => <>));
   end Create;

end Rx.Op.Observe_On;
