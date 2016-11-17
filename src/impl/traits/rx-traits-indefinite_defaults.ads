with Rx.Holders;
with Rx.Traits.Types;

generic
   type T (<>) is private; -- User type that is already definite;
package Rx.Traits.Indefinite_Defaults is

   package Holders is new Rx.Holders (T);

   type D is new Holders.Definite with null record;

   function To_Definite   (V : T) return D renames "+";
   function To_Indefinite (V : D) return T renames "+";

   package Type_Traits is new Traits.Types (T, D);

end Rx.Traits.Indefinite_Defaults;
