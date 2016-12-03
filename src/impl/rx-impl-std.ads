with Rx.Definites;
with Rx.Indefinites;
with Rx.Numeric_Observables;
with Rx.Numeric_Operators;
with Rx.Operators;

package Rx.Impl.Std is

   package Any      is new Rx.Indefinites (Rx_Any'Class);
   package Floats   is new Rx.Definites (Rx_Float);
   package Integers is new Rx.Definites (Rx_Integer);
   package Strings  is new Rx.Indefinites (Rx_String);

   package AnyToFlt is new Rx.Operators (Any.Observables,      Floats.Observables);
   package IntToFlt is new Rx.Operators (Integers.Observables, Floats.Observables);
   package StrToFlt is new Rx.Operators (Strings.Observables,  Floats.Observables);

   package AnyToInt is new Rx.Operators (Any.Observables,     Integers.Observables);
   package FltToInt is new Rx.Operators (Floats.Observables,  Integers.Observables);
   package StrToInt is new Rx.Operators (Strings.Observables, Integers.Observables);

   package AnyToStr is new Rx.Operators (Any.Observables,      Strings.Observables);
   package FltToStr is new Rx.Operators (Floats.Observables,   Strings.Observables);
   package IntToStr is new Rx.Operators (Integers.Observables, Strings.Observables);

   function To_Integer (I : Rx_Integer) return Rx_Integer is (I) with Inline;
   function To_Float   (I : Rx_Integer) return Rx_Float   is (Rx_Float (I)) with Inline;
   function Succ       (F : Rx_Float)   return Rx_Float   is (F + 1.0) with Inline;

   package Numeric is

   --  Encapsulate numeric observables

      package Integers is new Numeric_Observables (Integers.Observables,
                                                   To_Integer,
                                                   Rx_Integer'Succ);

      package Floats is new Numeric_Observables (Floats.Observables,
                                                 To_Float,
                                                 Succ);

      package Any_To_Int is new Rx.Numeric_Operators (AnyToInt,
                                                      To_Integer,
                                                      Rx_Integer'Succ);

      package Str_To_Int is new Rx.Numeric_Operators (StrToInt,
                                                      To_Integer,
                                                      Rx_Integer'Succ);

   end Numeric;

end Rx.Impl.Std;
