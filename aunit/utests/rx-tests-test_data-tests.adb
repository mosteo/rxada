--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Rx.Tests.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Rx.Tests.Test_Data.Tests is


--  begin read only
   procedure Test_Basic_Tests (Gnattest_T : in out Test);
   procedure Test_Basic_Tests_19bfa7 (Gnattest_T : in out Test) renames Test_Basic_Tests;
--  id:2.2/19bfa7c0f9557f4a/Basic_Tests/1/0/
   procedure Test_Basic_Tests (Gnattest_T : in out Test) is
   --  rx-tests.ads:5:4:Basic_Tests
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert (Basic_Tests, "");

--  begin read only
   end Test_Basic_Tests;
--  end read only


--  begin read only
   procedure Test_No_Op (Gnattest_T : in out Test);
   procedure Test_No_Op_c25040 (Gnattest_T : in out Test) renames Test_No_Op;
--  id:2.2/c25040a9472b436a/No_Op/1/0/
   procedure Test_No_Op (Gnattest_T : in out Test) is
   --  rx-tests.ads:7:4:No_Op
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert (No_Op, "");

--  begin read only
   end Test_No_Op;
--  end read only


--  begin read only
   procedure Test_Subscriptions (Gnattest_T : in out Test);
   procedure Test_Subscriptions_2e6fe1 (Gnattest_T : in out Test) renames Test_Subscriptions;
--  id:2.2/2e6fe1b5c0e01664/Subscriptions/1/0/
   procedure Test_Subscriptions (Gnattest_T : in out Test) is
   --  rx-tests.ads:9:4:Subscriptions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert (Subscriptions, "");

--  begin read only
   end Test_Subscriptions;
--  end read only

end Rx.Tests.Test_Data.Tests;