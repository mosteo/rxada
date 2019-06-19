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

   Sub : Rx.Subscriptions.Subscription with Unreferenced;
begin
   Put_Line ("Number of CPUs:" & System.Multiprocessors.Number_Of_CPUs'Img);

   --  Sequential listing & hashing of files
   Sub :=
     DirX.Observables.Directory_Entries (Target, Recursive => True)
     & Examples.Hash'Access
     & Subscribe (On_Next => Examples.Print_Hash'Access);

end DirX.Hash_Recursive;
