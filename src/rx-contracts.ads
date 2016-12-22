--  Interfaces that rule the Rx world

with Rx.Errors;
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
   procedure On_Completed (This : in out Observer) is abstract;
   procedure On_Error     (This : in out Observer; Error : Errors.Occurrence) is abstract;

   ----------------
   -- Subscriber --
   ----------------

   type Subscriber is interface and Observer;
   --  Someone capable of becoming uninterested on more data

   function Is_Subscribed (This : Subscriber) return Boolean is abstract;
   --  A subscriber can be interrogated about its desire for more data, to allow premature stop

   procedure Unsubscribe (This : in out Subscriber) is abstract;
   --  A subscriber can be marked as no longer interested in more data

   ----------------
   -- Observable --
   ----------------

   type Observable is interface;
   --  Someone capable of producing data to which an observer can subscribe

   procedure Subscribe (Producer : in out Observable;
                        Consumer :        Subscriber'Class) is abstract;

   ----------
   -- Sink --
   ----------

   -- Final Endpoint for a live chain
   type Sink is abstract new Subscriber with private;
   --  A sink is someone who requested a subscription and consumes data,
   --  as opposed to an operator that passed data along.

   overriding function Is_Subscribed (This : Sink) return Boolean;

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

   type Sink is abstract new Subscriber with record
      Subscription : Subscriptions.Subscription;
   end record;

   overriding function Is_Subscribed (This : Sink) return Boolean is
      (This.Subscription.Is_Subscribed);

end Rx.Contracts;
