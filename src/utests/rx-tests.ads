--  Explicit test functions, since gnattest mostly fails for generics

package Rx.Tests is

   function Misc_Tests return Boolean;

   function Operators return Boolean;

   function Subscriptions return Boolean;

   function Sources return Boolean;

end Rx.Tests;
