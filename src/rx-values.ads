with Rx.Holders;

package Rx.Values is

   pragma Preelaborate;

   type Value is interface;

   package Holders is new Rx.Holders (Value'Class);
   type Holder is new Holders.Definite with null record;

end Rx.Values;
