with Rx.Debug; use Rx.Debug;
with Rx.Debug.Observers;
with Rx.Impl.Semaphores;
with Rx.Std;   use Rx.Std;
with Rx.Schedulers;
with Rx.Subscriptions;

procedure Rx.Bugs.Testbed is
   use Integers;

   package Checkers is new Debug.Observers (Integers.Typedd, 0, Image);

   task type Dumper (Dumpee : access Integers.Sink);

   task body Dumper is
   begin
      for I in 1 .. 1000 loop
         Dumpee.On_Next (0);
      end loop;
   end Dumper;

   procedure Test_001_Shared_Leak with Unreferenced is
      S : Integers.Subscription;
      pragma Unreferenced (S);
   begin
      S :=
        From ((1, 2, 3, 4, 5)) &
        Limit (3) &
        Observe_On (Schedulers.Computation) &
        Subscribe (Debug.Put_Line'Access);
   end Test_001_Shared_Leak;

   procedure Test_002_Blocking with Unreferenced is
      Sem  : aliased Impl.Semaphores.Shared_Binary := Impl.Semaphores.Create;
      Crit : Impl.Semaphores.Critical_Section (Sem'Access) with Unreferenced;
   begin
      null;
   end Test_002_Blocking;

   procedure Test_003_Serialize with Unreferenced is
      Dumpee  : aliased Integers.Sink := Checkers.Subscribe_Count_Printer;
   begin
      declare
         Dumpers : array (1 .. 10) of access Dumper := (others => new Dumper (Dumpee => Dumpee'Access))
           with Unreferenced;
      begin
         null;
      end;
      -- After tasks completion, print:
      Dumpee.On_Completed;
   end Test_003_Serialize;

begin
   for I in 1 .. 1 loop
      Test_003_Serialize;
   end loop;

   Put_Line ("END");
end Rx.Bugs.Testbed;