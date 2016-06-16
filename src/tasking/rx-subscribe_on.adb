with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Subscribe_On is

   package Remote is new Dispatchers.Subscribe (Operate);

   type Op is new Operate.Transform.Operator with record
      Sched : Schedulers.Scheduler;
   end record;

   overriding procedure On_Next      (This : in out Op; V : Operate.T);

   overriding procedure Subscribe    (This : in out Op; Child : in out Operate.Observer);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
   begin
      This.Get_Child.On_Next (V);
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

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator is
   begin
      return Op'(Operate.Transform.Operator with Sched => Scheduler);
   end Create;

end Rx.Subscribe_On;
