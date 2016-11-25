with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Rx.Traits.Definite_Defaults;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Collections is

   --  Default collections for simple use of grouping operators

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Typed.T, Typed.Type_Traits."=");

   subtype List is Lists.List;

--     package List_Traits is new Rx.Traits.Definite_Defaults (List);


end Rx.Collections;
