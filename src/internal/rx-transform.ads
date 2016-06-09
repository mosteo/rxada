with Rx.Typed;

generic
   with package From is new Rx.Typed (<>); -- Naming chosen for same length
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   type Func1 is access function (V : From.Type_Traits.T) return Into.Type_Traits.T;

   type Operator is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
   with private;

   function "&" (L : From.Producers.Observable'Class;
                 R : Operator'Class)
                 return Into.Producers.Observable'Class;

   not overriding
   procedure On_Next (This  : in out Operator;
                      Child : in out Into.Consumers.Observer'Class;
                      V     : From.Type_Traits.T) is abstract;

   not overriding
   procedure On_Completed (This : in out Operator; Child : in out Into.Consumers.Observer'Class) is null;

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Consumers.Observer'Class);

   overriding
   procedure On_Next (This : in out Operator; V : From.Type_Traits.T);

   overriding
   procedure On_Completed (This : in out Operator);

private

   type Operator is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
   with record
      Child : Into.Consumers.Holder;
   end record;

end Rx.Transform;
