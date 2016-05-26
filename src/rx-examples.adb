with Rx.Debug;
with Rx.Subscribe;

package body Rx.Examples is

   ----------
   -- Test --
   ----------

   procedure Test is
      package Counter is new Rx.Subscribe (Pulse.Output, Rx.Debug.Put_Line);
   begin
      Rx.Debug.Put_Line ("Test");
   end Test;

end Rx.Examples;
