with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);

   type Indexes is (<>);
package Rx.Traits.Arrays is

   pragma Preelaborate;

   type Typed_Array is array (Indexes range <>) of Typed.Type_Traits.D;

end Rx.Traits.Arrays;
