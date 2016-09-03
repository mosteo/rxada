with Rx.Definites;
with Rx.Indefinites;
with Rx.Integers;
with Rx.Operators;
with Rx.Strings;
with Rx.Subscriptions;

package Rx.Examples is

   package StrToInt is new Rx.Operators (Strings.Observables, Integers.Observables);
   package IntToStr is new Rx.Operators (Integers.Observables, Strings.Observables);

   package IntCount is new Rx.Integers.Observables.Counters (Integer'Succ);
   package StrCount is new StrToInt.Counters (Integer'Succ);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);
   function Inc (I : Integer) return Integer is (I+1);

   Chain : Subscriptions.No_Subscription;

   --  Finally, to increase ambiguity:
--   package Chars is new Rx.Definites (Character); -- THIS LINE BREAKS SOMETHING

   type Intarr is array (Integer range <>) of Integer;
--     package Intarrs is new Rx.Indefinites (Intarr);

end Rx.Examples;
