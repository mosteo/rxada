with Ada.Containers.Indefinite_Holders;

generic
   type Indef (<>) is private;
package Rx.Holders is

   pragma Preelaborate;

   package Definites is new Ada.Containers.Indefinite_Holders (Indef);

   type Definite is new Definites.Holder with null record;

   function "+" (I : Indef) return Definite renames To_Holder;
   function "+" (D : Definite) return Indef renames Element;

   function Ref  (I : aliased in out Definite) return Definites.Reference_Type renames Reference;

   function CRef (I : aliased Definite) return Definites.Constant_Reference_Type renames Constant_Reference;

end Rx.Holders;
