with Rx.Typed;

generic
   with package From is new Rx.Typed (<>); -- Naming chosen for same lenght
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   type Func1 is access function (V : From.T) return Into.T;

   type Operator is abstract new
     Into.Producers.Observable and
     From.Producers.Subscriber and
     From.Consumers.Observer
   with private;

   overriding
   procedure Set_Parent (This : in out Operator; Parent : From.Producers.Observable'Class);

   overriding
   function  Get_Parent (This : Operator) return From.Producers.Observable'Class;

private

   type Operator is abstract new
     Into.Producers.Observable and
     From.Producers.Subscriber and
     From.Consumers.Observer
   with record
      Parent : From.Producers.Holder;
   end record;

end Rx.Transform;
