with Rx.Traits.Types;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Metatyped is

   package Traits is new Rx.Traits.Types (Typed.Contracts.Observable'Class,
                                          Typed.Definite_Observables.Observable,
                                          Typed.Definite_Observables.From,
                                          Typed.Definite_Observables.To_Indef);

   package Metainstance is new Rx.Typed (Traits);

   subtype Observable is Metainstance.Observable'Class;
   --  An observable that emits observables that emit the base type T

end Rx.Metatyped;
