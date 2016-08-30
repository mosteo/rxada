with Ada.Exceptions;

with Rx.Errors;
with Rx.Holders;

generic
   type T (<>) is private;
package Rx.Consumers is

   --     pragma Preelaborate;

   type Observer is interface;
   procedure On_Next      (This : in out Observer; V : T) is abstract;
   procedure On_Completed (This : in out Observer) is abstract;
   procedure On_Error     (This : in out Observer; Error : in out Errors.Occurrence) is abstract;

   --  I don't see the point of separating the subscription interface like RxJava does, since everywhere
   --  an observer is, a subscriber is expected to be too. Hence until some reason arises they're conflated.
--     procedure Unsubscribe (This : in out Observer) is abstract;
--     function IsSubscribed (This : Observer) return Boolean is abstract;

   type Sink is interface;
   --  Used to diagnose improper subscriptions to things not a terminator Sink.
   --  Any custom final subscriber must implement this interface.

   package Holders is new Rx.Holders (Observer'Class, "observer'class");
   type Holder is new Holders.Definite with null record;

   procedure Default_Error_Handler (This : in out Observer'Class; Except : Ada.Exceptions.Exception_Occurrence);

end Rx.Consumers;
