with Rx.Actions;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Subscribe is

   pragma Preelaborate;

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Completed : Rx.Actions.Proc0      := null;
                    On_Error     : Rx.Actions.Proc_Error := null) return Typed.Contracts.Sink'Class;

end Rx.Subscribe;
