with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Rx.Traits.Types;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Collections is

   --  Default collections for simple use of grouping operators

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Typed.T, Typed.Type_Traits."=");

   subtype List is Lists.List;


   --  Work-around for bug using Definite_Defaults
   function Identity (L : List) return List is (L) with Inline;

   package List_Traits is new Rx.Traits.Types (List, List, Identity, Identity);
   package Typed_Lists is new Rx.Typed (List_Traits);


end Rx.Collections;
