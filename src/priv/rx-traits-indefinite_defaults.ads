with Rx.Tools.Holders;
with Rx.Traits.Types;

generic
   type T (<>) is private; -- User type that is not definite;
package Rx.Traits.Indefinite_Defaults is

   package Holders is new Rx.Tools.Holders (T, "indefinite_defaults.T");

   type D is new Holders.Definite with null record;

--     function To_Definite   (V : T) return D renames "+";
--     function To_Indefinite (V : D) return T renames "+";

   package Type_Traits is new Traits.Types (T, D, Hold, Get);

end Rx.Traits.Indefinite_Defaults;
