with Rx.Tests;

procedure Rx.Examples.Tests is
begin
   pragma Assert (Rx.Tests.Basic_Tests);
   pragma Assert (Rx.Tests.No_Op);
   pragma Assert (Rx.Tests.Subscriptions);
   pragma Assert (Rx.Tests.Sources);
   pragma Assert (Rx.Tests.Operators);
end Rx.Examples.Tests;
