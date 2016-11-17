with Rx.Transform;

generic
   with package Typed is new Rx.Transform (<>);
package Rx.Op.Scan is

   function Create (Func : Typed.Func2;
                    Seed : Typed.Into.T;
                    Emit : Boolean      := False) -- If the seed has to be emitted
                    return Typed.Operator'Class;

end Rx.Op.Scan;
