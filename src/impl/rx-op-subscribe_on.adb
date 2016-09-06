with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Subscribe_On is

   package Remote is new Dispatchers.Subscribe (Operate);

   type Op is new Operate.Operator with record
      Sched : Schedulers.Scheduler;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T; Child : in out Operate.Observer'Class);

   overriding procedure Subscribe    (This : in out Op; Child : in out Operate.Observer);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T; Child : in out Operate.Observer'Class) is
      pragma Unreferenced (This);
   begin
      Child.On_Next (V);
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Child : in out Operate.Observer) is
   begin
      This.Set_Child (Child);
      -- Relay subscription to the actual thread:
      Remote.On_Subscribe (This.Sched.all, This);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class is
   begin
      return Op'(Operate.Operator with Sched => Scheduler);
   end Create;

end Rx.Op.Subscribe_On;
