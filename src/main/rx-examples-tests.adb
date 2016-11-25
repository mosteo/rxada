with GNAT.Exception_Traces;

with Rx.Tests;
with Rx.Typed.Meta;

procedure Rx.Examples.Tests is
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   pragma Assert (Rx.Tests.Basic_Tests);
   pragma Assert (Rx.Tests.No_Op);
   pragma Assert (Rx.Tests.Subscriptions);
   pragma Assert (Rx.Tests.Sources);
   pragma Assert (Rx.Tests.Operators);
end Rx.Examples.Tests;
