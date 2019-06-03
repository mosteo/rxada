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

   type Inflater1 is access function (V : From.T) return Into.Observable'Class;

   type TInflater1 is interface;
   function Evaluate (Func : TInflater1; V : From.T) return Into.Observable'Class is abstract;

   function Wrap (Func : Inflater1) return TInflater1'Class;

   package TInflater1_Holders is new Tools.Holders (TInflater1'Class);
   type HInflater1 is new TInflater1_Holders.Definite with null record;

private

   type Inflater1_Wrapper (Func : Inflater1) is new TInflater1 with null record;

   overriding function Evaluate (Func : Inflater1_Wrapper; V : From.T) return Into.Observable'Class is
     (Func.Func (V));

   function Wrap (Func : Inflater1) return TInflater1'Class is
     (Inflater1_Wrapper'(Func => Func));

end Rx.Actions.Transform;
