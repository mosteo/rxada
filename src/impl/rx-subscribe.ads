with Rx.Actions;
with Rx.Errors;
with Rx.Typed;

private with Rx.Subscriptions;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Subscribe is

   pragma Preelaborate;

   procedure Default_On_Error (E : Errors.Occurrence);

   type Proc_Error is access procedure (E : Errors.Occurrence);
   --  Needed to circumvent a forbidden 'Access use (RM 3.10.2(32))

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Completed : Rx.Actions.Proc0      := null;
                    On_Error     : Proc_Error            := Default_On_Error'Access)
                    return Typed.Contracts.Sink'Class;

   function Create (Using : Typed.Observer) return Typed.Sink;
   --  Wraps an observer into a Sink, providing subscription management

   type Observer is new Typed.Contracts.Observer with null record;
   --  This default observer can be used as base for Create.
   --  It provides a sane default for On_Error and does nothing otherwise

   overriding procedure On_Next (This : in out Observer; V : Typed.T) is null;

   overriding procedure On_Completed (This : in out Observer) is null;

   overriding procedure On_Error (This : in out Observer;
                                  E    :        Errors.Occurrence);

private

   --  Either the access to procedures are used (hence the class was created by Create)
   --  or the held observer is valid

   type Subscribe is new Typed.Contracts.Sink with record
      Func_On_Next      : Typed.Actions.Proc1;
      Func_On_Completed : Rx.Actions.Proc0;
      Func_On_Error     : Proc_Error;

      Observer : Typed.Holders.Observer;

      Completed    : Boolean := False;
      Errored      : Boolean := False;
   end record;

   overriding procedure On_Next      (This : in out Subscribe; V : Typed.T);
   overriding procedure On_Completed (This : in out Subscribe);
   overriding procedure On_Error     (This : in out Subscribe; Error : Errors.Occurrence);

end Rx.Subscribe;
