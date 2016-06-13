package body Rx.Observables is

   ---------
   -- "&" --
   ---------

   function "&" (L : Typed.Producers.Observable'Class; R : Typed.Consumers.Observer'Class)
                 return Subscriptions.Subscription
   is
      Actual_L : Typed.Producers.Observable'Class := L;
      Actual_R : Typed.Consumers.Observer'Class   := R;
   begin
      Actual_L.Subscribe (Actual_R);
      return Subscriptions.Subscription'(null record);
   end "&";

end Rx.Observables;
