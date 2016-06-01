with Ada.Containers.Indefinite_Holders;

with Rx.Values;

package Rx.Holder is

   use type Rx.Values.Value;

   package  Holder is new Ada.Containers.Indefinite_Holders (Values.Value'Class);
   type     TH     is new Holder.Holder with null record;
   function Hold (V : Values.Value'Class) return TH renames To_Holder;

end Rx.Holder;
