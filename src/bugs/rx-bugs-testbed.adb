with Rx.Debug; use Rx.Debug;
with Rx.Impl.Semaphores;
with Rx.Std;   use Rx.Std;
with Rx.Schedulers;
with Rx.Subscriptions;

procedure Rx.Bugs.Testbed is
   use Integers;

   procedure Test_001_Shared_Leak is
      S : Integers.Subscription;
      pragma Unreferenced (S);
   begin
      S :=
        From ((1, 2, 3, 4, 5)) &
        Limit (3) &
        Observe_On (Schedulers.Computation) &
        Subscribe (Debug.Put_Line'Access);
   end Test_001_Shared_Leak;

   procedure Test_002_Blocking is
      Sem  : aliased Impl.Semaphores.Binary;
      Crit : Impl.Semaphores.Critical_Section (Sem'Access);
   begin
      null;
   end Test_002_Blocking;

begin
   for I in 1 .. 1 loop
      Test_002_Blocking;
   end loop;

   Put_Line ("END");

--   Dump;
end Rx.Bugs.Testbed;
