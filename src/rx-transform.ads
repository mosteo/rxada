with Rx.Observable;
with Rx.Actions;

generic
   with package Producer is new Rx.Observable (<>);
   with package Consumer is new Rx.Observable (<>);
package Rx.Transform is

   type Func1 is access function (V : Producer.T) return Consumer.T;

   function Func (F : Func1) return Rx.Actions.Func1'Class;

end Rx.Transform;
