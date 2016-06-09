with Rx.Observables;
with Rx.Traits.Types;
with Rx.Typed;

-- Entry point for a user to declare a new Rx-processed type, with full traits control
-- For simpler instantiations take a look at Rx.Definites and Rx.Indefinites
generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Types is

   package Typed is new Rx.Typed (Type_Traits);

   --  This is the package to be use'd in user code
   package Observables is new Rx.Observables (Typed);

   subtype Observable is Typed.Producers.Observable'Class;

end Rx.Types;
