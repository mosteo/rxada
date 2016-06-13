-- with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
--  This is a workaround for a memory leak in the Indefinite_Holders (as of GPL2016)

generic
   type Indef (<>) is private;
package Rx.Holders is

   pragma Preelaborate;

   package Definites is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Indef);

   type Definite is tagged private;

   function "+" (I : Indef)    return Definite with Inline;
   function "+" (D : Definite) return Indef    with Inline;

   function Hold (I : Indef) return Definite renames "+";

   function Ref  (I : aliased in out Definite) return Definites.Reference_Type          with Inline;
   function CRef (I :                Definite) return Definites.Constant_Reference_Type with Inline;

private

   type Definite is new Definites.List with null record;

   function "+" (D : Definite) return Indef is (D.Constant_Reference (D.First));

end Rx.Holders;
