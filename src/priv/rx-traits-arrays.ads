with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);

   type Indexes is (<>);
package Rx.Traits.Arrays is

--     pragma Preelaborate;

   type Typed_Array is array (Indexes range <>) of Typed.Type_Traits.D;

   Empty_Array : constant Typed_Array;

   -- HERESY TO AVOID FORCING THE USER TO DESIGNATE A NULL VALUE

   function Build (V1 : Typed.T; More : Typed_Array := Empty_Array) return Typed_Array;
   function Build (V1, V2 : Typed.T; More : Typed_Array := Empty_Array) return Typed_Array;
   function Build (V1, V2, V3 : Typed.T; More : Typed_Array := Empty_Array) return Typed_Array;
   function Build (V1, V2, V3, V4 : Typed.T; More : Typed_Array := Empty_Array) return Typed_Array;
   function Build (V1, V2, V3, V4, V5 : Typed.T; More : Typed_Array := Empty_Array) return Typed_Array;
   function Build (V1, V2, V3, V4, V5, V6 : Typed.T; More : Typed_Array := Empty_Array) return Typed_Array;

private

   Empty_Array : constant Typed_Array (Indexes'Succ (Indexes'First) .. Indexes'First) := (others => <>);

end Rx.Traits.Arrays;
