with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.Do_On is

   function Create (On_Next : Preserver.Typed.Actions.TProc1'Class)
                    return Preserver.Operator'Class;

end Rx.Op.Do_On;
