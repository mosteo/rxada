package body Rx.Subscriptions is

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (S : in out Subscription) is
      procedure Set (B : in out State) is
      begin
         B := Unsubscribed;
      end Set;
   begin
      if S.Is_Valid then
         S.Apply (Set'Access);
      else
         raise Program_Error with "Was never subscribed";
      end if;
   end Unsubscribe;

end Rx.Subscriptions;
