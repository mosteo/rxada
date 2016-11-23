package body Rx.Subscriptions is

   function Is_Subscribed (S : in out Subscription) return Boolean is
      Result : Boolean;

      procedure Get (B : in out State) is
      begin
         Result := B = Subscribed;
      end Get;
   begin
      if S.Is_Valid then
         S.Apply (Get'Access);
         return Result;
      else
         raise Program_Error with "Was never subscribed";
      end if;
   end Is_Subscribed;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (S : in out Subscription) is
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
