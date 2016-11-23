with Rx.Errors;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Impl.Shared_Subscriber with Preelaborate is

   type Subscriber is new Typed.Contracts.Subscriber with private;
   --  In essence this is a carcass for a pointed to observer.
   --  This way, both threads using it access the same actual Observer.
   --  Deallocation is properly done in On_Completed/On_Error

   --  This expects proper serialization of calls, hence is not thread-safe.
   --  See Operator Serialize for a safeguard for cases where this is not true

   function Create (Held : Typed.Subscriber) return Subscriber;
   procedure Release (This : in out Subscriber);

   overriding procedure On_Next      (This : in out Subscriber; V : Typed.Type_Traits.T);
   overriding procedure On_Completed (This : in out Subscriber);
   overriding procedure On_Error     (This : in out Subscriber; Error : in out Errors.Occurrence);

   overriding function Is_Subscribed (This : in Subscriber) return Boolean;

   overriding procedure Unsubscribe  (This : in out Subscriber);

   Null_Observer : constant Subscriber;

private

   type Subscriber_Access is access Typed.Subscriber;

   type Subscriber is new Typed.Contracts.Subscriber with record
      Actual : Subscriber_Access;
   end record;

   overriding function Is_Subscribed (This : in Subscriber) return Boolean is
      (This.Actual /= null and then This.Actual.Is_Subscribed);

   Null_Observer : constant Subscriber := (Typed.Contracts.Subscriber with Actual => null);

end Rx.Impl.Shared_Subscriber;
