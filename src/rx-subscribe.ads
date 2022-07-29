with Rx.Actions;
with Rx.Errors;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Subscribe is

   pragma Preelaborate;

   type Proc_Error is access procedure (E : Errors.Occurrence);
   --  Needed to circumvent a forbidden 'Access use (RM 3.10.2(32))

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Complete  : Rx.Actions.Proc0      := null;
                    On_Error     : Proc_Error            := Typed.Defaults.Default_On_Error'Access)
                    return Typed.Contracts.Sink'Class;

   function Create (Using : Typed.Observer'Class) return Typed.Sink;
   --  Wraps an observer into a Sink, providing subscription management
   --  See Typed.Defaults.Observer for a possible base implementation

private

   --  Either the access to procedures are used (hence the class was created by Create)
   --  or the held observer is valid

   type Subscribe is new Typed.Contracts.Sink with record
      Func_On_Next      : Typed.Actions.Proc1;
      Func_On_Complete  : Rx.Actions.Proc0;
      Func_On_Error     : Proc_Error;

      Observer : Typed.Holders.Observer;

      Completed    : Boolean := False;
      Errored      : Boolean := False;
   end record;

   overriding procedure On_Next      (This : in out Subscribe; V : Typed.T);
   overriding procedure On_Complete  (This : in out Subscribe);
   overriding procedure On_Error     (This : in out Subscribe; Error : Errors.Occurrence);

end Rx.Subscribe;
