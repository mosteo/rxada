with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Observe_On is

   package Remote is new Dispatchers.Events (Operate.Typed);
   package Shared renames Remote.Shared;

   type Op is new Operate.Operator with record
      Scheduler  : Schedulers.Scheduler;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T);
   overriding procedure On_Complete  (This : in out Op);
   overriding procedure On_Error     (This : in out Op; Error : Errors.Occurrence);

   overriding procedure Subscribe    (This : in out Op; Observer : in out Operate.Into.Observer'Class);

   function Get_Downstream (This : in out Op'Class) return Shared.Observer is
     (if This.Get_Observer /= null then -- There's a bug here on creation of the reference
         Shared.Observer (This.Get_Observer.all)
      else
         raise No_Longer_Subscribed);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
   begin
      if not Get_Downstream (This).Is_completed then
         Remote.On_Next (This.Scheduler.all, Get_Downstream (This), V);
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Op) is
   begin
      Remote.On_Complete  (This.Scheduler.all, Get_Downstream (This));
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : Errors.Occurrence) is
   begin
      Remote.On_Error (This.Scheduler.all, Get_Downstream (This), Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Observer : in out Operate.Into.Observer'Class) is
      Actual : Shared.Observer := Shared.Create (Observer);
   begin
      Operate.Operator (This).Subscribe (Actual);
      --  Get_Observer not to be used directly, so this subscription could use an always failing observer
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class is
   begin
      return Op'(Operate.Operator with
                                 Scheduler  => Scheduler);
   end Create;

end Rx.Op.Observe_On;
