pragma Detect_Blocking;
pragma License (Modified_GPL);

package Rx with Pure is

   Unimplemented : exception;
   --  Used to signal features that are on the roadmap, but not yet completed

   No_Longer_Subscribed : exception;
   --  This is the only subscription pervading this design.
   --  Generators of data must be aware that this can be raised in any observer call.

   --  The following are defaults for the default operators.
   --  Also they are used in operators that take regular numbers as parameters.
   --  It is a compromise to eliminate yet another generic parameter which will rarely be meaningul.
   --  For specific need the user can create instances for any desired type.

   subtype Rx_Integer  is Long_Long_Integer;
   subtype Rx_Natural  is Rx_Integer range 0 .. Rx_Integer'Last;
   subtype Rx_Positive is Rx_Integer range 1 .. Rx_Integer'Last;

   subtype Rx_Float   is Long_Long_Float;

   subtype Rx_String  is String;

   type    Rx_Nothing is null record;
   --  Some observables are used for notification purposes, with values of no importance

   --  Other literals of general use

   type Rx_Event_Kinds is
     (On_Next,
      On_Complete,
      On_Error);

   type Merge_Policies is
     (Merge, 	-- Just relay as they come
      Sequence, -- Force sequencing of observables
      Switch);  -- Drop from any previous observable, use only last one

end Rx;
