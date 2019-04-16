with Rx.Traits.Definite_Defaults;
with Rx.Types;

-- Entry point for a user to declare a new type to be used in Rx chains
generic
   type T is private;
package Rx.Definites is

   -- Preparation instances
   package Defaults is new Rx.Traits.Definite_Defaults (T);
   package Instance is new Rx.Types (Defaults.Type_Traits);

   -- Actually usable package
   package Observables renames Instance.Observables;

   -- Other shortcuts
   package Contracts renames Instance.Typed.Contracts;

   subtype Observable is Instance.Observable;

end Rx.Definites;
