with Rx.Operators;

generic
   with package Ops is new Rx.Operators (<>); -- Items to be counted and into what
package Rx.Counters is

   -- Specializable counter
   generic
      with function Succ (V : Ops.Into.Typed.Type_Traits.T) return Ops.Into.Typed.Type_Traits.T;
   function Discrete_Count (First : Ops.Into.Typed.Type_Traits.T)
                            return Ops.Typed.Operator'Class;

end Rx.Counters;
