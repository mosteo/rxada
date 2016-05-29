package body Rx.Map is

   Instance : aliased Operator;

   ------------
   -- OnNext --
   ------------

   overriding procedure OnNext
     (This : in out Operator;
      V : Input.T)
   is
   begin
      for Subscriber of This.Subscribers loop
         declare
            R : constant Result := Transform (V);
         begin
            Subscriber.OnNext (R);
         end;
      end loop;
   end OnNext;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (O : in out Operator;
      S : access Output.Observer'Class)
   is
   begin
      O.Subscribers.Append (S);
      Input.Instance.Subscribe(O'Access);
   end Subscribe;

begin
   Output.Instance := Instance'Access;
end Rx.Map;
