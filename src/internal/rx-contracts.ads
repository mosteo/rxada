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

   ----------
   -- Sink --
   ----------

   type Sink is interface;
   --  A sink is someone who requested a subscription and consumes data,
   --  as opposed to an operator that passed data along.

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

   end Typed;


--     type Descendant is interface;
--     --  Type that can be put in a chain
--     --  This probably should go straight to Links
--
--     procedure Set_Parent (This : in out Descendant; Parent : Observable'Class) is abstract;
--     function  Get_Parent (This :        Descendant) return Observable'Class is abstract;
--     function  Has_Parent (This :        Descendant) return Boolean is abstract;

end Rx.Contracts;