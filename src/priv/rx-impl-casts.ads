with Ada.Strings;	use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package Rx.Impl.Casts is

   --  Casts between the standard Ada types for use in the Cast operator

   function To_Integer (V : Rx_Float)  return Rx_Integer is (Rx_Integer (V));
   function To_Integer (V : Rx_String) return Rx_Integer is (Rx_Integer'Value (V));

   function To_Float (V : Rx_Integer) return Rx_Float is (Rx_Float (V));
   function To_Float (V : Rx_String)  return Rx_Float is (Rx_Float'Value (V));

   function To_String (V : Rx_Integer) return String is (Trim (Rx_Integer'Image (V), Both));
   function To_String (V : Rx_Float)   return String is (Trim (Rx_Float'Image (V), Both));
   function To_String (V : Rx_String)  return String is (V);
   --  A default conversion with 4 decimal digits

end Rx.Impl.Casts;
