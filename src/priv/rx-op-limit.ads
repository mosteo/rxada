with Rx.Preservers;

generic
   with package Operate is new Rx.Preservers (<>);
package Rx.Op.Limit is

   function Create (Limit : Rx_Natural) return Operate.Operator'Class;
   --  If limit is 0, On_Completed will be called upon first On_Next (but not instantly at subscription time).
   --  This is according to RxJava implementation, there is no explicit behavior in Rx specs.

end Rx.Op.Limit;
