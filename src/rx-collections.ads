with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Rx.Traits.Types;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Collections is

   -------------
   --  Lists  --
   -------------

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Typed.T, Typed.Type_Traits."=");
   subtype List is Lists.List;

   --  Work-around for bug using Definite_Defaults
   function Identity (L : List) return List is (L) with Inline;

   package List_Traits is new Rx.Traits.Types (List, List, Identity, Identity);
   package Typed_Lists is new Rx.Typed (List_Traits);

   -------------------------------
   --  Emission of observables  --
   -------------------------------

   package Observable_Traits is new Rx.Traits.Types (Typed.Contracts.Observable'Class,
                                                     Typed.Definite_Observables.Observable,
                                                     Typed.Definite_Observables.From,
                                                     Typed.Definite_Observables.To_Indef);

   package Typed_Observables is new Rx.Typed (Observable_Traits);


end Rx.Collections;
