with Ada.Containers.Indefinite_Holders;

generic
   type T (<>) is private;
package Rx.Holder is

--  Helpers for indefinite values

   package  Holder is new Ada.Containers.Indefinite_Holders (T);
   type     TH     is new Holder.Holder with null record;
   function Hold (V : T) return TH renames To_Holder;

end Rx.Holder;
