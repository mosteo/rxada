with Rx.Traits.Types;
with Rx.Typed;

package Rx.Valueless with Preelaborate is

   type Nothing is null record;

   function To_Definite   (V : Nothing) return Nothing is (V) with Inline;
   function To_Indefinite (V : Nothing) return Nothing is (V) with Inline;

   package Traits is new Rx.Traits.Types (Nothing, Nothing);

   package Typed is new Rx.Typed (Traits);

end Rx.Valueless;
