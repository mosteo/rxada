with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Indef (<>) is private;
package Rx.Holders is

   pragma Preelaborate;

   package Definites is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Indef);

   type Definite is new Definites.List with null record;

   function "+" (I : Indef) return Definite;

   function To_Definite (I : Indef) return Definite renames "+";

   function Element  (I : Definite) return Indef;

end Rx.Holders;
