with Rx.Transform;

generic
   with package Transform is new Rx.Transform (<>); -- Items to be counted and into what
   with function Succ (V : Transform.Into.T) return Transform.Into.T;
package Rx.Count is

   function Count (First : Transform.Into.T)
                   return  Transform.Operator'Class;

end Rx.Count;
