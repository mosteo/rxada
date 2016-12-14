with Rx.Debug; use Rx.Debug;

package body Rx.Bugs.Support is

   -------
   -- X --
   -------


   task body X is
      Reaper : Reap.Reaper (X'Unchecked_Access);
   begin
      Put_Line ("Terminated? " & Reaper.Victim.all'Terminated'Img);
   end X;

   task body Y is
      Reaper : Impl.Tasks.Reaper (Y'Unchecked_Access) with Unreferenced;
   begin
      Put_Line ("Terminated? " & Y'Terminated'Img);
   end Y;

end Rx.Bugs.Support;
