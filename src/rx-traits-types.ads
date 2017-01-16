pragma Warnings (Off);

generic
   type T (<>) is private; -- The user-facing type
   type D      is private; -- Definite type for storage of T
   with function To_Definite   (V : T) return D is <>;
   with function To_Indefinite (V : D) return T is <>;
package Rx.Traits.Types with Preelaborate is

--     function "+" (V : T) return D renames To_Definite;
--     function "-" (V : D) return T renames To_Indefinite;

end Rx.Traits.Types;
