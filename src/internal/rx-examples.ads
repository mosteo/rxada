with Rx.Integers;
with Rx.Operators;
with Rx.Strings;
with Rx.Subscriptions;

package Rx.Examples is

   package StrToInt is new Rx.Operators (Strings, Integers);
   package IntToStr is new Rx.Operators (Integers, Strings);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);

   Chain : Subscriptions.Subscription;

end Rx.Examples;
