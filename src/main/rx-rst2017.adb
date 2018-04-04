with Rx.Debug; use Rx.Debug;
With Rx.Schedulers;
with Rx.Std; use Rx.Std;
with Rx.Subscriptions;

with GNAT.String_Hash;

package body Rx.RST2017 is

   use Integers;
   use Integer_To_String;
   use String_To_Integer;

   type Hashes is mod 2 ** 32;

   function Modular_Hash is new GNAT.String_Hash.Hash (Character, Rx_String, Hashes);

   function String_Hash (S : String) return Rx_Integer is (Rx_Integer (Modular_Hash (S)));

   function Image (I : Rx_Integer) return String is (Rx_Integer'Image (I));

   S : constant Subscription :=
         Interval (First => 1, Period => 1.0) &
   -- The RxAda Interval observable uses Duration as the time unit, and uses Ada tasks to implement Rx threads
         Observe_On (Schedulers.Computation) &
   -- Switch to a computation task
         Map (Image'Access) &
   -- Function that takes an Integer and returns its String image
         Map (String_Hash'Access) &
   -- E.g. instance of System.String_Hash
         Observe_On (Schedulers.IO) &
   -- Switch to an Input/Output thread
         Subscribe (Put_Line'Access) with Unreferenced;

end Rx.RST2017;
