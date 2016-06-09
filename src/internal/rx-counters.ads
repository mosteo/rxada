with Rx.Operators;

generic
   with package Operators is new Rx.Operators (<>); -- Items to be counted and into what
   with function Succ (V : Operators.Into.Typed.Type_Traits.T) return Operators.Into.Typed.Type_Traits.T;
package Rx.Counters is

   function Count (First : Operators.Into.Typed.Type_Traits.T)
                   return Operators.Typed.Operator'Class;

end Rx.Counters;
