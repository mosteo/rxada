with Rx.Typed;

generic
   with package From is new Rx.Typed (<>); -- Naming chosen for same lenght
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   type Func1 is access function (V : From.T) return Into.T;

   type Operator is abstract new
     From.Producers.Observable and
     Into.Consumers.Observer
   with null record;

end Rx.Transform;
