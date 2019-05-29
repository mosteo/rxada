--  Interfaces that rule the Rx world

with Rx.Errors;
with Rx.Subscribers;
with Rx.Subscriptions;

generic
   type T (<>) is private;
package Rx.Contracts is

   pragma Preelaborate;

   --------------
   -- Observer --
   --------------

   type Observer is interface;
   --  Someone interested in receiving data

   procedure On_Next      (This : in out Observer; V : T) is abstract;
   procedure On_Complete  (This : in out Observer) is abstract;
   procedure On_Error     (This : in out Observer; Error : Errors.Occurrence) is abstract;

   ----------------
   -- Observable --
   ----------------

   type Observable is interface;
   --  Someone capable of producing data to which an observer can subscribe

   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Observer'Class) is abstract;

   ----------------
   -- Subscriber --
   ----------------

   subtype Subscriber is Subscribers.Subscriber;

   ----------
   -- Sink --
   ----------

   -- Final Endpoint for a live chain
   type Sink is abstract new Observer and Subscribers.Subscriber with private;
   --  A sink is someone who requested a subscription and consumes data,
   --  as opposed to an operator that passes data along.

   overriding function Is_Subscribed (This : Sink) return Boolean;

   overriding procedure On_Complete (This : in out Sink);
   --  Call this if overrinding it

   not overriding
   procedure Set_Subscription (This : in out Sink; S : Subscriptions.Subscription);
   --  A sink receives a subscription at the moment of being subscribed

   overriding procedure Unsubscribe (This : in out Sink);

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (Producer : Observable'Class; Consumer : Sink'Class) return Subscriptions.Subscription;
   --  Execute the subscription

private

   type Sink is abstract new Observer and Subscribers.Subscriber with record
      Subscription : Subscriptions.Subscription;
   end record;

   overriding function Is_Subscribed (This : Sink) return Boolean is
      (This.Subscription.Is_Subscribed);

end Rx.Contracts;
