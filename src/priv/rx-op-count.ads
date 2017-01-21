with Rx.Impl.Transformers;

generic
   with package Transform is new Rx.Impl.Transformers (<>); -- Items to be counted and into what
   with function Succ (V : Transform.Into.T) return Transform.Into.T;
   Default_Initial_Count : Transform.Into.T;
package Rx.Op.Count is

   function Count (First : Transform.Into.T := Default_Initial_Count)
                   return  Transform.Operator'Class;

end Rx.Op.Count;
