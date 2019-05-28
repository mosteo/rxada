with Rx.Traits.Indefinite_Defaults;
with Rx.Types;

-- Entry point for a user to declare a new type to be used in Rx chains
generic
   type T (<>) is private;
package Rx.Indefinites is

   -- Preparation instances
   package Defaults is new Rx.Traits.Indefinite_Defaults (T);
   package Instance is new Rx.Types (Defaults.Type_Traits);

   -- Actually usable package
   package Observables renames Instance.Observables;

   -- Other shortcuts
   package Actions   renames Instance.Typed.Actions;
   package Contracts renames Instance.Typed.Contracts;

   subtype Observable is Instance.Observable;

   --  For easy visibility of this heresy:
   package Arrays renames Observables.Default_Arrays;

end Rx.Indefinites;
