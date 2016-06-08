with Rx.Counters;

generic
   with function Succ (V : Typed.Into.Type_Traits.T) return Typed.Into.Type_Traits.T;
package Rx.Operators.Counters is

   package Into_Counters is new Rx.Counters (

end Rx.Operators.Counters;
