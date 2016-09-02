with Rx.Traits.Types;

generic
   type T is private; -- User type that is already definite;
package Rx.Traits.Definite_Defaults is

   type D is new T;

   function To_Definite   (V : T) return D is (D (V)) with Inline;
   function To_Indefinite (V : D) return T is (T (V)) with Inline;

   package Type_Traits is new Traits.Types (T, D);

end Rx.Traits.Definite_Defaults;
