with Ada.Containers.Indefinite_Holders;

with Rx.Interfaces;

package Rx.Holder is

   use type Rx.Interfaces.Value;

   package  Holder is new Ada.Containers.Indefinite_Holders (Interfaces.Value'Class);
   type     TH     is new Holder.Holder with null record;
   function Hold (V : Interfaces.Value'Class) return TH renames To_Holder;

end Rx.Holder;
