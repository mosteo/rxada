with Rx.Dispatchers;
with Rx.Errors;

-- with Gnat.Io; use Gnat.Io;

package body Rx.Op.Subscribe_On is

   package Remote is new Dispatchers.Subscribe (Operate);

   --  This special in that, since it interrupts the subscription chain, can't be implemented with
   --  the usual Implementation.Operator
   type Op is new Operate.Operator with record
      Sched : Schedulers.Scheduler;
   end record;

   overriding procedure Subscribe    (This : in out Op; Observer : Operate.Into.Subscriber);

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Op; Observer : Operate.Into.Subscriber) is
   begin
      -- Relay subscription to the actual thread:
      Remote.On_Subscribe (This.Sched.all, This.Get_Parent, Observer);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (Scheduler : Schedulers.Scheduler) return Operate.Operator'Class is
   begin
      return Op'(Operate.Operator with Sched => Scheduler);
   end Create;

end Rx.Op.Subscribe_On;
