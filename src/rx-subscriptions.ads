private with Rx.Impl.Shared_Data;

package Rx.Subscriptions is

   pragma Preelaborate;

   No_Longer_Subscribed : exception;

   type Subscription is tagged private;

   function Subscribe return Subscription;

   procedure Unsubscribe (S : in out Subscription);

   function Is_Subscribed (S : Subscription) return Boolean;

   -- For when we do not care at all:

   type No_Subscription is null record;

   function "-" (S : Subscription) return No_Subscription is (null record);

private

   type State is (Subscribed, Unsubscribed);

   type State_Access is access State;

   --  This probably can be done more lightweight since it involves a single boolean check
   package Shared_Booleans is new Rx.Impl.Shared_Data (State, State_Access);

   type Subscription is new Shared_Booleans.Proxy with null record;

   function Is_Subscribed (S : Subscription) return Boolean is (S.Is_Valid and then S.Get = Subscribed);

   function Subscribe return Subscription is (Wrap (new State'(Subscribed)));

end Rx.Subscriptions;
