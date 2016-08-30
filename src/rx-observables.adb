package body Rx.Observables is

   ---------
   -- "&" --
   ---------

   function "&" (L : Typed.Producers.Observable'Class; R : Typed.Consumers.Observer'Class)
                 return Subscriptions.No_Subscription
   is
      Actual_L : Typed.Producers.Observable'Class := L;
      Actual_R : Typed.Consumers.Observer'Class   := R;
   begin
      if not (R in Typed.Consumers.Sink'Class) then
         raise Constraint_Error with "Attempting to subscribe from non-sink observer";
      end if;

      Actual_L.Subscribe (Actual_R);
      return Subscriptions.No_Subscription'(null record);
   end "&";

end Rx.Observables;
