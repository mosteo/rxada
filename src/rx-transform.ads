with Rx.Typed;

generic
   with package From is new Rx.Typed (<>); -- Naming chosen for same lenght
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   type Func1 is access function (V : From.T) return Into.T;

   type Operator is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
   with private;

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Consumers.Observer'Class);

   overriding
   procedure OnNext (This : in out Operator; V : From.T);

   not overriding
   procedure On_Next (This  : in out Operator;
                      Child : in out Into.Consumers.Observer'Class;
                      V     : From.T) is abstract;

   function "&" (L : From.Producers.Observable'Class;
                 R : Operator'Class)
                 return Into.Producers.Observable'Class;

private

   type Operator is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
   with record
      Child : Into.Consumers.Holder;
   end record;

end Rx.Transform;
