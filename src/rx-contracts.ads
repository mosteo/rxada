--  Interfaces that rule the Rx world

with Rx.Errors;
with Rx.Subscriptions;

package Rx.Contracts is

   pragma Preelaborate;

   ----------------
   -- Subscriber --
   ----------------

   type Subscriber is interface;
   --  Someone capable of becoming uninterested on more data

   function Is_Subscribed (This : Subscriber) return Boolean is abstract;
   --  A subscriber can be interrogated about its desire for more data, to allow premature stop

   generic
      type T (<>) is private;
   package Typed is

      --------------
      -- Observer --
      --------------

      type Observer is interface;
      --  Someone interested in receiving data

      procedure On_Next      (This : in out Observer; V : T) is abstract;
      procedure On_Completed (This : in out Observer) is abstract;
      procedure On_Error     (This : in out Observer; Error : in out Errors.Occurrence) is abstract;

      ----------------
      -- Observable --
      ----------------

      type Observable is interface;
      --  Someone capable of producing data to which an observer can subscribe

      procedure Subscribe (Producer : in out Observable;
                           Consumer : in out Observer'Class) is abstract;

      ----------
      -- Sink --
      ----------

      -- Final Endpoint for a live chain
      type Sink is interface and Observer and Subscriber;
      --  A sink is someone who requested a subscription and consumes data,
      --  as opposed to an operator that passed data along.

      function Get_Subscription (S : Sink) return Subscriptions.Subscription is abstract;

   end Typed;

end Rx.Contracts;
