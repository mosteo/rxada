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
   procedure On_Error     (This : in out Observer; Error : in out Errors.Occurrence) is abstract;

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
                        Consumer : in out Subscriber'Class) is abstract;

   ----------
   -- Sink --
   ----------

   -- Final Endpoint for a live chain
   type Sink is interface and Subscriber;
   --  A sink is someone who requested a subscription and consumes data,
   --  as opposed to an operator that passed data along.

   function Get_Subscription (S : Sink) return Subscriptions.Subscription is abstract;
   --  A sink must asynchronously allow a way of stopping incoming data

end Rx.Contracts;
