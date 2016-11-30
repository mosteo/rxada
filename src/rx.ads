pragma License (Modified_GPL);

pragma Detect_Blocking;

package Rx with Pure is

   type    Rx_Any      is interface;
   --  By deriving user types from Rx_Any, users can benefit from some Operators in Rx.Std by default

   --  The following are defaults for the default operators.
   --  For specific need the user can create instances for any desired type.

   subtype Rx_Integer  is Long_Long_Integer;
   subtype Rx_Natural  is Rx_Integer range 0 .. Rx_Integer'Last;
   subtype Rx_Positive is Rx_Integer range 1 .. Rx_Integer'Last;

   subtype Rx_Float   is Long_Long_Float;

   subtype Rx_String  is String;

end Rx;
