with Rx.Transformers;

generic
   with package Typed is new Rx.Transformers (<>);
package Rx.Op.Scan is

   function Create (Func : Typed.Actions.Func2;
                    Seed : Typed.Into.T;
                    Emit : Boolean      := False) -- If the seed has to be emitted
                    return Typed.Transformer'Class;

end Rx.Op.Scan;
