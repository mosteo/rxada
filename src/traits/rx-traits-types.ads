pragma Warnings (Off);

generic
   type T (<>) is private; -- The user-facing type
   type D      is private; -- Definite type for storage of T
   with function To_Definite   (V : T) return D is <>;
   with function To_Indefinite (V : D) return T is <>;
package Rx.Traits.Types is
end Rx.Traits.Types;
