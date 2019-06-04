with Rx.Debug; use Rx.Debug;
with Rx.Std;   use Rx.Std;
with Rx.Schedulers;
with Rx.Schedulers.Pools;

procedure Rx.Examples.Threading is
   use Integers;
--     use Strings;
--     use Integer_To_String;
--     use String_To_Integer;

   Custom_Pool : Schedulers.Pools.Pool := Schedulers.Pools.Create (Size => 2, Name => "Custom");

   function Custom_Idle return Schedulers.Thread is (Custom_Pool.Get_Idle);
   function Custom_Next return Schedulers.Thread is (Custom_Pool.Get_Next);

   procedure Finish is
   begin
      Debug.Put_Line ("Shutting down...");
   end Finish;

begin
   Sub :=
    Std.Interval
     & Limit (5)
     & Print
     & Subscribe_On (Schedulers.IO)
     & Observe_On (Schedulers.Idle_Thread)
     & Print
     & Observe_On (Schedulers.New_Thread)
     & Print
     & Observe_On (Schedulers.Computation)
     & Print
     & Observe_On (Schedulers.To_Scheduler (Custom_Next'Unrestricted_Access))
     & Print
     & Observe_On (Schedulers.To_Scheduler (Custom_Next'Unrestricted_Access))
     & Print
     & Observe_On (Schedulers.To_Scheduler (Custom_Idle'Unrestricted_Access))
     & Print
     & Subscribe (On_Next     => Put_Line'Access,
                  On_Complete => Finish'Unrestricted_Access);
   --  Regular accesses would suffice at library level

exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
