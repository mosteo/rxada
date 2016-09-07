--  Interfaces that rule the Rx world

with Rx.Errors;

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

      procedure Observe (Producer : in out Observable;
                         Consumer : in out Observer'Class) is abstract;
      --  This is Subscribe in RxJava, but I'm tired of inconsistent naming over there


      ----------
      -- Sink --
      ----------

      -- Final Endpoint for a live chain
      type Sink is interface and Observer and Subscriber;
      --  A sink is someone who requested a subscription and consumes data,
      --  as opposed to an operator that passed data along.

   end Typed;

end Rx.Contracts;
