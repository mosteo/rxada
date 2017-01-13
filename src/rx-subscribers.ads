package Rx.Subscribers with Pure is

   type Subscriber is interface;

   procedure Unsubscribe (S : in out Subscriber) is abstract;

   function Is_Subscribed (S : Subscriber) return Boolean is abstract;

end Rx.Subscribers;
