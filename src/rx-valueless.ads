with Rx.Traits.Types;
with Rx.Impl.Typed;

package Rx.Valueless with Preelaborate is

   Subtype Nothing is Rx_Nothing;

   function To_Definite   (V : Nothing) return Nothing is (V);
   function To_Indefinite (V : Nothing) return Nothing is (V);

   package Traits is new Rx.Traits.Types (Nothing, Nothing);

   package Typed is new Rx.Impl.Typed (Traits);

   subtype Observable is Typed.Observable;

end Rx.Valueless;
