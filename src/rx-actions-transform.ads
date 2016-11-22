with Rx.Contracts;

generic
   with package From is new Rx.Contracts (<>);
   with package Into is new Rx.Contracts (<>);
package Rx.Actions.Transform with Preelaborate is

   --  Plain type transforming functions

   type Func1 is access function (V : From.T) return Into.T;

   type Func2 is access function (F : From.T;
                                  I : Into.T) return Into.T;

   --  FlatMap infrastructure

   type Flattener1 is access function (V : From.T) return Into.Observable'Class;

end Rx.Actions.Transform;
