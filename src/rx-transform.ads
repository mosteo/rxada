with Rx.Observable;

generic
   with package Producer is new Rx.Observable (<>);
   with package Consumer is new Rx.Observable (<>);
package Rx.Transform is

   type Func1 is access function (V : Producer.T) return Consumer.T;

end Rx.Transform;
