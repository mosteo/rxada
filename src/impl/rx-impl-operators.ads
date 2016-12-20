with Rx.Errors;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Impl.Operators with Preelaborate is

   type Operator is abstract new From.Contracts.Subscriber and Into.Contracts.Observable with private;
   --  This is the type recommended to override for implementing new operators
   --  In conjunction with the Create function here, it provides a proper transformer
   --  with correct management of subscriptions and errors, and proper defaults

      --  Defaults to be overriden per operator

   overriding procedure On_Next (This : in out Operator; V : From.T) is null;
   --  Just drops the value

   overriding procedure On_Completed (This : in out Operator);
   --  Just passes it along

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence);
   --  Just passes it along

   overriding procedure Subscribe (This : in out Operator; Observer : Into.Subscriber'Class);
   --  Override only if you need a modified observer (for example a Shared one)
   --  In that case, call this base class statically with the new observer


   --  From this point on there should be no need to override the rest of methods unless for specific
   --  operators that tinker with these events

   function Get_Subscriber (This : in out Operator'Class) return Into.Holders.Subscribers.Reference;
   --  Use this in order to get the downstream observer

   overriding function Is_Subscribed (This : Operator) return Boolean;
   --  Proper default, no need to override

   overriding procedure Unsubscribe (This : in out Operator);
   --  Proper default, no need to override

   not overriding procedure Clear (This : in out Operator);
   --  Clear any resources being held

private

   type Operator is abstract new From.Contracts.Subscriber and Into.Contracts.Observable with record
      Downstream : Into.Holders.Subscriber;
   end record;

   overriding function Is_Subscribed (This : Operator) return Boolean is (This.Downstream.Is_Valid);

   function Get_Subscriber (This : in out Operator'Class) return Into.Holders.Subscribers.Reference is
     (This.Downstream.Ref);

end Rx.Impl.Operators;
