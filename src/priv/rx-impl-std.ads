with Rx.Definites;
with Rx.Indefinites;
with Rx.Numeric_Observables;
with Rx.Numeric_Operators;
with Rx.Operators;
with Rx.Valueless;

package Rx.Impl.Std is

   package Floats   is new Rx.Definites (Rx_Float);
   package Integers is new Rx.Definites (Rx_Integer);
   package Strings  is new Rx.Indefinites (Rx_String);
   package Nothings is new Rx.Definites (Valueless.Nothing);

   package Int_To_Float is new Rx.Operators (Integers.Observables, Floats.Observables);
   package String_To_Float is new Rx.Operators (Strings.Observables,  Floats.Observables);

   package Float_To_Integer is new Rx.Operators (Floats.Observables,  Integers.Observables);
   package String_To_Integer is new Rx.Operators (Strings.Observables, Integers.Observables);

   package Float_To_String is new Rx.Operators (Floats.Observables,   Strings.Observables);
   package Integer_To_String is new Rx.Operators (Integers.Observables, Strings.Observables);

   function To_Integer (I : Rx_Integer) return Rx_Integer is (I);
   function To_Float   (I : Rx_Integer) return Rx_Float   is (Rx_Float (I));
   function Succ       (F : Rx_Float)   return Rx_Float   is (F + 1.0);

   package Numeric is

   --  Encapsulate numeric observables

      package Integers is new Numeric_Observables (Integers.Observables,
                                                   To_Integer,
                                                   Rx_Integer'Succ);

      package Floats is new Numeric_Observables (Floats.Observables,
                                                 To_Float,
                                                 Succ);

      package Str_To_Int is new Rx.Numeric_Operators (String_To_Integer,
                                                      To_Integer,
                                                      Rx_Integer'Succ);

   end Numeric;

end Rx.Impl.Std;
