with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with DirX.Examples;
with DirX.Observables;

with Rx.Subscriptions;

with System.Multiprocessors;

procedure DirX.Hash_Recursive is
   use Ada.Command_Line;

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

   Sub : Rx.Subscriptions.Subscription with Unreferenced;
begin
   Put_Line ("Number of CPUs:" & System.Multiprocessors.Number_Of_CPUs'Img);

   --  Sequential listing & hashing of files
--     Sub :=
--       DirX.Observables.Directory_Entries (Target, Recursive => True)
--       & Examples.Hash'Access
--       & Subscribe (On_Next => Examples.Print_Hash'Access);

   --  Sequential timing
   Sub :=
     DirX.Observables.Directory_Entries (Target, Recursive => True)
     & Examples.Hash'Access
     & Stopwatch (Inspect'Unrestricted_Access)
     & Subscribe;

end DirX.Hash_Recursive;
