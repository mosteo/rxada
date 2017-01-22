with Rx.Observables;
with Rx.Traits.Types;
with Rx.Impl.Typed;

-- Entry point for a user to declare a new Rx-processed type, with full traits control
-- For simpler instantiations take a look at Rx.Definites and Rx.Indefinites
generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Types is

   --  This is the parametric package to instance other packages provided by Rx
   --  Not usually needed if using Rx.Std or default Observables
   package Typed is new Rx.Impl.Typed (Type_Traits);

   --  This is the package to be used in plain user code
   package Observables is new Rx.Observables (Typed);

   subtype Observable is Typed.Contracts.Observable'Class;

end Rx.Types;
