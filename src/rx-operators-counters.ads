with Rx.Count;

generic
   with function Succ (V : Into.T) return Into.T;
package Rx.Operators.Counters is

   package Pkg_Count is new Rx.Count (Typed, Succ);

   function Count (First : Into.T)
                   return Typed.Operator'Class renames Pkg_Count.Count;

end Rx.Operators.Counters;
