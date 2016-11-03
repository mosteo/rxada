with Rx.Actions;
with Rx.Errors;
with Rx.Typed;

private with Rx.Subscriptions;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Subscribe is

   pragma Preelaborate;

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Completed : Rx.Actions.Proc0      := null;
                    On_Error     : Rx.Actions.Proc_Error := null) return Typed.Contracts.Sink'Class;

   type Subscribe is new Typed.Contracts.Sink with private;
   --  As an alternative you can override this convenience class...
   --  Override these that follow as needed
   --  But DON'T override the ones provided in Contracts.Observer if you want to retain default
   --  checked behavior

   not overriding
   procedure Do_On_Next      (This : in out Subscribe; V : Typed.T) is null;

   not overriding
   procedure Do_On_Completed (This : in out Subscribe) is null;

   not overriding
   procedure Do_On_Error     (This : in out Subscribe; Error : in out Errors.Occurrence) is null;

private

   --  Either the access to procedures are used (hence the class was created by Create)
   --  or the overriden members are called (that might be null as well anyway...)

   type Subscribe is new Typed.Contracts.Sink with record
      Func_On_Next      : Typed.Actions.Proc1;
      Func_On_Completed : Rx.Actions.Proc0;
      Func_On_Error     : Rx.Actions.Proc_Error;

      Subscription : Subscriptions.Subscription := Subscriptions.Subscribe;
      --  These defaults are necessary in case this type is used directly instead of Create'd

      Completed    : Boolean := False;
      Errored      : Boolean := False;
   end record;

   overriding procedure On_Next      (This : in out Subscribe; V : Typed.T);
   overriding procedure On_Completed (This : in out Subscribe);
   overriding procedure On_Error     (This : in out Subscribe; Error : in out Errors.Occurrence);

   overriding function Is_Subscribed (This : Subscribe) return Boolean is (This.Subscription.Is_Subscribed);

   overriding function Get_Subscription (This : Subscribe) return Subscriptions.Subscription is (This.Subscription);

end Rx.Subscribe;
