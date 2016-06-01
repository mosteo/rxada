with Rx.I;
with Rx.Producers.Composite;

package Rx.Operators is

   type Operator is abstract new Producers.Composite.Observable and I.S.Observer with null record;

   overriding
   procedure OnNext (This : Operator; V : I.V.Value'Class);
   --  This will dispatch on all subscribers, so shouldn't be overriden lightly

end Rx.Operators;
