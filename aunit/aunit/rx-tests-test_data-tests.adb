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

      AUnit.Assertions.Assert
        (Basic_Tests,
         "Basic tests FAILED.");

--  begin read only
   end Test_Basic_Tests;
--  end read only

end Rx.Tests.Test_Data.Tests;
