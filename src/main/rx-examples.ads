with Rx.Counters;
with Rx.Integers;
with Rx.Operators;
with Rx.Strings;
with Rx.Subscriptions;

package Rx.Examples is

   package StrToInt is new Rx.Operators (Strings.Observables, Integers.Observables);
   package IntToStr is new Rx.Operators (Integers.Observables, Strings.Observables);
   package IntToInt is new Rx.Operators (Integers.Observables, Integers.Observables);

   package IntCount is new Rx.Counters (IntToInt, Integer'Succ);
   package StrCount is new Rx.Counters (StrToInt, Integer'Succ);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);
   function Inc (I : Integer) return Integer is (I+1);

   Chain : Subscriptions.Subscription;

end Rx.Examples;
