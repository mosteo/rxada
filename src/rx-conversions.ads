with Rx.Traits.Types;

generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Conversions with Preelaborate is

   subtype D is Type_Traits.D;
   subtype T is Type_Traits.T;

   function "=" (L, R : T) return Boolean renames Type_Traits."=";

   function "+" (V : T) return D renames Type_Traits.To_Definite;
   function "+" (V : D) return T renames Type_Traits.To_Indefinite;

   function Def (V : T) return D renames Type_Traits.To_Definite;
   function Ind (V : D) return T renames Type_Traits.To_Indefinite;

end Rx.Conversions;
