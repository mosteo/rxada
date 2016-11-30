pragma License (Modified_GPL);

pragma Detect_Blocking;

package Rx with Pure is

   type    Rx_Any      is interface;
   --  By deriving user types from Rx_Any, users can benefit from some Operators in Rx.Std by default

   subtype Rx_Integer  is Integer;
   subtype Rx_Natural  is Rx_Integer range 0 .. Rx_Integer'Last;
   subtype Rx_Positive is Rx_Integer range 1 .. Rx_Integer'Last;

   subtype Rx_Float   is Float;

   subtype Rx_String  is String; -- Should this better be Wide_Wide_String???

end Rx;
