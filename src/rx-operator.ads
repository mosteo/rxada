with Rx.Producer;

--  An operator transforms or produces data

generic
   type T (<>) is private;
package Rx.Operator is

   pragma Preelaborate;

   package Binding is new Rx.Producer (T);
   subtype Observable is Binding.Observable;
   subtype Observer   is Binding.Observer;

   Instance : access Binding.Observable'Class;

end Rx.Operator;
