with Rx.Bugs.Support;
with Rx.Debug; use Rx.Debug;
with Rx.Debug.Observers;
with Rx.Tools.Semaphores;
with Rx.Std;   use Rx.Std;
with Rx.Schedulers;
with Rx.Subscriptions;

procedure Rx.Bugs.Testbed is
   use Integers;

   package Checkers is new Debug.Observers (Integers.Typed, 0, Image);

   task type Dumper (Dumpee : access Integers.Sink);

   type Dumper_Access is access all Dumper;

   task body Dumper is
   begin
      for I in 1 .. 1000 loop
         Dumpee.On_Next (0);
      end loop;
   end Dumper;

   procedure Test_001_Shared_Leak with Unreferenced is
      -- There should be no leak here
      S : Subscription;
      pragma Unreferenced (S);
   begin
      S :=
        From ((1, 2, 3, 4, 5)) &
        Limit (3) &
        Observe_On (Schedulers.Computation) &
        Subscribe (Debug.Put_Line'Access);
   end Test_001_Shared_Leak;

   procedure Test_002_Blocking with Unreferenced is
      -- There should be no blocking operation exception here
      Sem  : aliased Tools.Semaphores.Shared := Tools.Semaphores.Create_Reentrant;
      Crit : Tools.Semaphores.Critical_Section (Sem'Access) with Unreferenced;
   begin
      null;
   end Test_002_Blocking;

   Dumpee  : aliased Integers.Sink := Checkers.Subscribe_Count_Printer;

   procedure Test_003_Serialize with Unreferenced is
      --  Check for serialize efectiveness
   begin
      declare
         Dumpers : array (1 .. 10) of Dumper_Access :=
                     (others => new Dumper (Dumpee => Dumpee'Access))
           with Unreferenced;
      begin
         null;
      end;
      -- After tasks completion, print:
      Dumpee.On_Complete ;
   end Test_003_Serialize;

begin
   for I in 1 .. 99 loop
      null;
   end loop;

   Put_Line ("END");
end Rx.Bugs.Testbed;
