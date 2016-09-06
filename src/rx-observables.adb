package body Rx.Observables is

   ---------
   -- "&" --
   ---------

   function "&" (L : Observable; R : Subscriptor)
                 return Subscriptions.Subscription
   is
      Actual_L : Typed.Producers.Observable'Class := L;
      Actual_R : Subscriptor := R; -- Typed.Consumers.Observer'Class (R);
   begin
      if not (R in Typed.Consumers.Sink'Class) then
         raise Program_Error with "Attempting to subscribe from non-sink observer";
      end if;

      Actual_R.Subscribe;
      Actual_L.Subscribe (Actual_R);
      return Actual_R.Subscription;
   end "&";

end Rx.Observables;
