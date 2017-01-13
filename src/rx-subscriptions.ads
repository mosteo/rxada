with Rx.Subscribers;

private with Rx.Impl.Shared_Data;

package Rx.Subscriptions is

   pragma Preelaborate;

   No_Longer_Subscribed : exception renames Rx.No_Longer_Subscribed;
   pragma Temporary;

   type Subscription is new Subscribers.Subscriber with private;

   function Subscribe return Subscription;

   overriding procedure Unsubscribe (S : in out Subscription);

   overriding function Is_Subscribed (S : Subscription) return Boolean;

   -- For when we do not care at all:

   type No_Subscription is null record;

   function "-" (S : Subscription) return No_Subscription is (null record);

private

   type State is (Subscribed, Unsubscribed);

   type State_Access is access State;

   --  This probably can be done more lightweight since it involves a single boolean check
   package Shared_Booleans is new Rx.Impl.Shared_Data (State, State_Access);

   type Subscription is new Shared_Booleans.Proxy and Subscribers.Subscriber with null record;

   overriding function Is_Subscribed (S : Subscription) return Boolean is (S.Is_Valid and then S.Get = Subscribed);

   function Subscribe return Subscription is (Wrap (new State'(Subscribed)));

end Rx.Subscriptions;
