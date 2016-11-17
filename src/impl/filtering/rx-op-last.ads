with Rx.Operate;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.Op.Last with Preelaborate is

   function Create (Check : Operate.Typed.Actions.TFilter1'Class := Operate.Typed.Actions.Always_Pass)
                    return Operate.Operator'Class;
   --  If limit is 0, On_Completed will be called upon first On_Next (but not instantly at subscription time).
   --  This is according to RxJava implementation, there is no explicit behavior in Rx specs.

end Rx.Op.Last;
