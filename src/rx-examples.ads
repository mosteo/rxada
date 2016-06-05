with Rx.Observable;
with Rx.Operators;

package Rx.Examples is

   package Strings  is new Rx.Observable (String);
   package Integers is new Rx.Observable (Integer);
   package StrToInt is new Rx.Operators (Strings, Integers);
   package IntToStr is new Rx.Operators (Integers, Strings);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);

end Rx.Examples;
