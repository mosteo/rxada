--  Explicit test functions, since gnattest mostly fails for generics

package Rx.Tests is

   function Basic_Tests return Boolean;

   function No_Op return Boolean;

   function Subscriptions return Boolean;

   function Sources return Boolean;

   function Operators return Boolean;

end Rx.Tests;
