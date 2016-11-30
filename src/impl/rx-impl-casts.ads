with Ada.Strings;	use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package Rx.Impl.Casts with Preelaborate is

   --  Casts between the standard Ada types for use in the Cast operator

   function To_Integer (V : Float)  return Rx_Integer is (Rx_Integer (V));
   function To_Integer (V : String) return Rx_Integer is (Rx_Integer'Value (V));

   function To_Float (V : Rx_Integer) return Float is (Float (V));
   function To_Float (V : String)  return Float is (Float'Value (V));

   function To_String (V : Rx_Integer) return String is (Trim (Rx_Integer'Image (V), Both));
   function To_String (V : Float)   return String is (Trim (Float'Image (V), Both));

end Rx.Impl.Casts;
