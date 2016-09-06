with Rx.Consumers;
with Rx.Holders;
with Rx.Subscriptions;

generic
   type T (<>) is private;
package Rx.Producers is

--     pragma Preelaborate;

   package Consumers is new Rx.Consumers (T);

   type Observable is interface;

   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Consumers.Observer'Class) is abstract;

   package Holders is new Rx.Holders (Observable'Class, "observable'class");
   type Holder is new Holders.Definite with null record;

   type Subscriber is interface;

   procedure Set_Parent (This : in out Subscriber; Parent : Observable'Class) is abstract;
   function  Get_Parent (This :        Subscriber) return Observable'Class is abstract;
   function  Has_Parent (This :        Subscriber) return Boolean is abstract;

   --  I don't see the point of separating the subscription interface like RxJava does, since everywhere
   --  an observer is, a subscriber is expected to be too. Hence until some reason arises they're conflated.
   procedure Subscribe     (This : in out Subscriber) is abstract;
   procedure Unsubscribe   (This : in out Subscriber) is abstract;
   function  Is_Subscribed (This :        Subscriber) return Boolean is abstract;
   function  Subscription  (This :        Subscriber) return Subscriptions.Subscription is abstract;
   procedure Share         (This : in out Subscriber; S : Subscriptions.Subscription) is abstract;

   -- Convenience type since we'll need all observers to also be subscribers
   type Subscriptor is abstract new Consumers.Observer and Subscriber with private;

   overriding procedure Set_Parent (This : in out Subscriptor; Parent : Observable'Class);
   overriding function  Get_Parent (This :        Subscriptor) return Observable'Class;
   overriding function  Has_Parent (This :        Subscriptor) return Boolean;

   overriding procedure Subscribe     (This : in out Subscriptor);
   overriding procedure Unsubscribe   (This : in out Subscriptor);
   overriding function  Is_Subscribed (This :        Subscriptor) return Boolean;
   overriding function  Subscription  (This :        Subscriptor) return Subscriptions.Subscription;
   overriding procedure Share         (This : in out Subscriptor; S : Subscriptions.Subscription);

private

   type Subscriptor is abstract new Consumers.Observer and Subscriber with record
      Parent       : Holder;
      Subscription : Subscriptions.Subscription;
   end record;

   overriding function  Get_Parent (This : Subscriptor) return Observable'Class is (This.Parent.Cref);
   overriding function  Has_Parent (This : Subscriptor) return Boolean is (not This.Parent.Is_Empty);

   overriding function  Is_Subscribed (This : Subscriptor) return Boolean
   is (This.Subscription.Is_Subscribed);

   overriding function  Subscription  (This : Subscriptor) return Subscriptions.Subscription
   is (This.Subscription);

end Rx.Producers;
