with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Rx.Bugs.Support;
with Rx.Debug; use Rx.Debug;
with Rx.Debug.Observers;
with Rx.Impl.Semaphores;
with Rx.Std;   use Rx.Std;
with Rx.Schedulers;
with Rx.Subscriptions;

procedure Rx.Bugs.Testbed is
   use Integers;

   package Checkers is new Debug.Observers (Integers.Typed, 0, Image);

   task type Dumper (Dumpee : access Integers.Sink);

   task body Dumper is
   begin
      for I in 1 .. 1000 loop
         Dumpee.On_Next (0);
      end loop;
   end Dumper;

   procedure Test_001_Shared_Leak with Unreferenced is
      -- There should be no leak here
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
      -- There should be no blocking operation exception here
      Sem  : aliased Impl.Semaphores.Shared_Binary := Impl.Semaphores.Create;
      Crit : Impl.Semaphores.Critical_Section (Sem'Access) with Unreferenced;
   begin
      null;
   end Test_002_Blocking;

   procedure Test_003_Serialize with Unreferenced is
      --  Check for serialize efectiveness
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

   procedure Test_004_Task_Leak with Unreferenced is
   --  Check of old gnat leak with every finished task
   --  It seems not: it has been fixed even if you free prior to 'Terminated
   --  (but then you're freeing the local task data)
   --  Conclusion: a synchronized root type could serve

      task type X (I : Integer);
      task body X is
      begin
         Put_Line (">>>" & I'Img);
         delay 1.0;
         Put_Line ("<<<" & I'Img);
      end X;

      type Ptr is access X;
      procedure Free is new Ada.Unchecked_Deallocation (X, Ptr);

      Arr : array (1 .. 9) of Ptr;

   begin
      for I in Arr'Range loop
         Arr (I) := new X (I);
      end loop;
      for I in Arr'Range loop
         while not Arr(I)'Terminated loop delay 0.001; end loop;
         Free (Arr (I));
         Put_Line ("FFF" & I'Img);
      end loop;
   end Test_004_Task_Leak;

   procedure Test_005_Task_Scope with Unreferenced is
      type Cont is new Ada.Finalization.Limited_Controlled with null record;
      overriding procedure Finalize (C : in out Cont) is
         pragma Unreferenced (C);
      begin
         Put_Line ("Cont out");
      end Finalize;

   --  Check that Controlled is called at task end
      task type X;
      task body X is
         C : Cont with Unreferenced;
         Self : access X := X'Access with Unreferenced;
      begin
         Put_Line ("X out");
      end X;

      XX : X;

   begin
      Put_Line ("T005 out");
   end Test_005_Task_Scope;

   procedure Test_006_Reaping with Unreferenced is
   -- Verifies that task reaping (via Tasks) works properly
      XX : Support.Y_Ptr := new Support.Y with Unreferenced;
   begin
      null;
   end Test_006_Reaping;

   procedure Test_007_Reaping_WA with Unreferenced is
   -- Verifies that task reaping (via Task_Deallocation) works properly
      XX : Support.X_Ptr := new Support.X with Unreferenced;
   begin
      null;
   end Test_007_Reaping_WA;

begin
   for I in 1 .. 999 loop
      Test_006_Reaping;
   end loop;

   Put_Line ("END");
end Rx.Bugs.Testbed;
