with Rx.Observable;
with Rx.Transform;

package Rx.Examples is

   package Strings  is new Rx.Observable (String);
   package Integers is new Rx.Observable (Integer);
   package StrToInt is new Rx.Transform (Strings, Integers);
   package IntToStr is new Rx.Transform (Integers, Strings);

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);

end Rx.Examples;
