package body Rx.Observables is

   ---------
   -- "&" --
   ---------

   function "&" (Producer : Observable; Consumer : Sink) return Subscriptions.Subscription
   is
      Actual_L : Observable := Producer;
      Actual_R : Sink       := Consumer;
      --  We create copies to start chain instantiation with fresh links
   begin
      Actual_L.Subscribe (Actual_R);
      return Actual_R.Get_Subscription;
   end "&";

end Rx.Observables;
