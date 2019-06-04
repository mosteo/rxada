with Rx.Debug;
with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Observe_On is

   package Remote is new Dispatchers.Events (Operate.Typed);
   package Shared renames Remote.Shared;

   type Op is new Operate.Operator with record
      Scheduler  : Schedulers.Scheduler;
      Thread     : Schedulers.Thread;
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
      Debug.Trace ("on_next");
      Remote.On_Next (This.Thread.all, This.Subscriber, V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Op) is
   begin
      Debug.Trace ("on_complete");
      Remote.On_Complete  (This.Thread.all, This.Subscriber);
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Op; Error : Errors.Occurrence) is
   begin
      Debug.Trace ("on_error");
      Remote.On_Error (This.Thread.all, This.Subscriber, Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Observer : in out Operate.Into.Observer'Class) is
   begin
      This.Subscriber := Shared.Create (Observer, Checked => False);
      --  Not our business to check integrity, there are plenty others doing it,
      --  and this one is heavily used by merge/flatmap, which rely on uncheckedness

      This.Thread := This.Scheduler.Get_Thread;

      Operate.Operator (This).Subscribe (This.Subscriber);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class is
   begin
      return Op'(Operate.Operator with
                   Scheduler  => Scheduler,
                   Thread     => <>,
                   Subscriber => <>); -- To be set during subscription
   end Create;

end Rx.Op.Observe_On;
