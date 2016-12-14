with Rx.Debug; use Rx.Debug;

package body Rx.Bugs.Support is

   task body Y is
      Reaper : Impl.Tasks.Reaper (Y'Unchecked_Access) with Unreferenced;
      X : Integer := 41 with Volatile; -- Ensure not optimized away
   begin
      delay 1.0;
      X := X + 1; -- Touch some memory
      Put_Line ("Meaning of life is" & X'Img);
   end Y;

end Rx.Bugs.Support;
