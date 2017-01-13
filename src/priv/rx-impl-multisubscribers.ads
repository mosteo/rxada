with Rx.Typed;

generic
   with package Typed_1 is new Rx.Typed (<>);
   with package Typed_2 is new Rx.Typed (<>);
package Rx.Impl.Multisubscribers with Preelaborate is

   --  A type that can observe several observables at the same time

   type Subscriber is tagged private;

   function As_Observer (This : in out Subscriber) return Typed_1.Subscriber'Class;
   function As_Observer (This : in out Subscriber) return Typed_2.Subscriber'Class;
   --  Get an observer view for each of the allowed types
   --  NOTE: this increases the observable-subscribed-into count, so use it only for actual subscriptions

   function Is_

private

   type Subscriber is tagged null record;

end Rx.Impl.Multisubscribers;
