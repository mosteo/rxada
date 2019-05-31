with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Rx.Impl.Preservers;
with Rx.Traits.Types;
with Rx.Impl.Transformers;
with Rx.Impl.Typed;
with Rx.Valueless;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Collections is

   -- Instances of types and transformations that we get automatically when creating a new Rx type

   -----------------
   --  Valueless  --
   -----------------

   package Valueless is new Impl.Transformers (Typed, Valueless.Typed);

   -------------------------
   --  Emission of Lists  --
   -------------------------

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Typed.T, Typed.Type_Traits."=");
   subtype List is Lists.List;

   function Identity (L : List) return List is (L);

   package List_Traits is new Rx.Traits.Types (List, List, Identity, Identity);
   package Typed_Lists is new Rx.Impl.Typed (List_Traits);

   -------------------------------
   --  Emission of observables  --
   -------------------------------

   package Observable_Traits is new Rx.Traits.Types (Typed.Contracts.Observable'Class,
                                                     Typed.Definite_Observables.Observable,
                                                     Typed.Definite_Observables.From,
                                                     Typed.Definite_Observables.To_Indef);

   package Typed_Observables is new Rx.Impl.Typed (Observable_Traits);

   -----------------
   --  Operators  --
   -----------------

   package List_Preservers        is new Rx.Impl.Preservers  (Typed_Lists);
   package Into_List_Transformers is new Rx.Impl.Transformers (Typed, Typed_Lists);
   package From_List_Transformers is new Rx.Impl.Transformers (Typed_Lists, Typed);

   package Obs_Transformers  is new Rx.Impl.Transformers (Typed, Typed_Observables);

end Rx.Collections;
