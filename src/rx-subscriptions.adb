package body Rx.Subscriptions is

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe return Subscription is
   begin
      return Wrap (new Boolean'(True));
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (S : in out Subscription) is
      procedure Set (B : in out Boolean) is
      begin
         B := True;
      end Set;
   begin
      S.Apply (Set'Access);
   end Unsubscribe;

   -------------------
   -- Is_Subscribed --
   -------------------

   function Is_Subscribed (S : in out Subscription) return Boolean is
      Result : Boolean;
      procedure Get (B : in out Boolean) is
      begin
         Result := B;
      end Get;
   begin
      S.Apply (Get'Access);
      return Result;
   end Is_Subscribed;

end Rx.Subscriptions;
