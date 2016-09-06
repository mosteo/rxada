with Rx.Tests;

procedure Rx.Examples.Tests is
begin
--   Enable_Symbolic_Traceback;

   pragma Assert (Rx.Tests.Basic_Tests);
   pragma Assert (Rx.Tests.No_Op);
   pragma Assert (Rx.Tests.Subscriptions);
end Rx.Examples.Tests;
