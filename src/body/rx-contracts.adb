package body Rx.Contracts is

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete (This : in out Sink) is
   begin
      This.Subscription.Unsubscribe;
   end On_Complete;

   ----------------------
   -- Set_Subscription --
   ----------------------

   procedure Set_Subscription (This : in out Sink; S : Subscriptions.Subscription) is
   begin
      This.Subscription := S;
   end Set_Subscription;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe
     (Producer : Observable'Class;
      Consumer : Sink'Class)
      return Subscriptions.Subscription
   is
      Actual_L : Observable'Class := Producer;
      Actual_R : Sink'Class       := Consumer;
      --  We create copies to start chain instantiation with fresh links
      Sub      : constant Subscriptions.Subscription := Subscriptions.Subscribe;
   begin
      Actual_R.Set_Subscription (Sub);
      Actual_L.Subscribe (Actual_R);
      return Sub;
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Sink) is
   begin
      This.Subscription.Unsubscribe;
   end Unsubscribe;

end Rx.Contracts;
