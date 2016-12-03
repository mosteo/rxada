package body Rx.Contracts is

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
   begin
      Actual_L.Subscribe (Actual_R);
      return Actual_R.Get_Subscription;
   end Subscribe;

end Rx.Contracts;
