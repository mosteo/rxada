with GNAT.Exception_Traces;

with Rx.Tests;

procedure Rx.Examples.Tests is
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);
   --  GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   --  Might be useful for debugging

   pragma Assert (Rx.Tests.Misc_Tests);
   pragma Assert (Rx.Tests.Subscriptions);
   pragma Assert (Rx.Tests.Sources);
   pragma Assert (Rx.Tests.Operators);

   delay 2.0; -- Wait for watchdogs
end Rx.Examples.Tests;
