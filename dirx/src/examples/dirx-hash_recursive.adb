with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with DirX.Examples;
with DirX.Observables;

with Rx.Schedulers;
with Rx.Std;
with Rx.Subscriptions;

with System.Multiprocessors;

procedure DirX.Hash_Recursive is
   use Ada.Command_Line;

   use Observables;
   use Observables.RxEntries.Observables;
   use Examples.RxHashed.Observables;
   use Examples.Entry_To_Hash;

   Target : constant Path := (if Argument_Count = 0
                              then "."
                              else Argument (1));

   Context : String (1 .. 10) := "Sequential";

   -------------
   -- Inspect --
   -------------

   procedure Inspect (Kind               : Rx.Rx_Event_Kinds;
                      Since_Previous     : Duration;
                      Since_Subscription : Duration)
   is
      pragma Unreferenced (Since_Previous);
      use all type Rx.Rx_Event_Kinds;
   begin
      if Kind = On_Complete then
         New_Line;
         Put_Line ("Wall time [" & Context & "]:" & Since_Subscription'Img);
      else
         null;
         --  Put_Line ("Incr time [" & Context & "]:" & Since_Previous'Img);
      end if;
   end Inspect;

   Sub : Rx.Subscriptions.Subscription;
begin
   Put_Line ("Number of CPUs:" & System.Multiprocessors.Number_Of_CPUs'Img);

   --  Sequential listing & hashing of files
--     Sub :=
--       DirX.Observables.Directory_Entries (Target, Recursive => True)
--       & Examples.Hash'Access
--       & Subscribe (On_Next => Examples.Print_Hash'Access);

   --  Sequential timing
   Sub :=
     Directory_Entries (Target, Recursive => True)
     & Examples.Hash'Access
     & Stopwatch (Inspect'Unrestricted_Access)
     & Rx.Std.String_To_Integer.Counters.Count
     & Subscribe;

   --  Parallel hashing timing
   Context := "Concurrent";
   Sub :=
     Directory_Entries (Target, Recursive => True)
     & Flat_Map (Observe_On (Rx.Schedulers.Computation)
                 & Examples.Hash'Access)
     & Stopwatch (Inspect'Unrestricted_Access)
     & Subscribe;

   while Sub.Is_Subscribed loop
      delay 0.1;
   end loop;

   --  Parallel enumeration and hashing
   Sub :=
     Directory_Entries (Target, Recursive => False)
     & Expand (Observe_On (Rx.Schedulers.Computation)
               & Dirx.Observables.Observe'Access)
     & Flat_Map (Observe_On (Rx.Schedulers.Computation)
                 & Examples.Hash'Access)
     & Stopwatch (Inspect'Unrestricted_Access)
     & Subscribe; -- (On_Next => Examples.Print_Hash'Access);

   while Sub.Is_Subscribed loop
      delay 0.1;
   end loop;

end DirX.Hash_Recursive;
