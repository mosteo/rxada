package body Rx.Count is

   Instance : aliased Operator;

   overriding procedure OnNext (This : in out Operator; V : Input.T) is
   begin
      This.Count := This.Count + 1;
   end OnNext;

   overriding procedure OnCompleted (This : in out Operator) is
   begin
      for Subscriber of This.Subscribers loop
         Subscriber.OnNext (This.Count);
         Subscriber.OnCompleted;
      end loop;
   end OnCompleted;

   overriding procedure Subscribe (O : in out Operator;
                        S : access Output.Observer'Class) is
   begin
      O.Subscribers.Append (S);
      Input.Instance.Subscribe(O'Access);
   end Subscribe;

begin
   Output.Instance := Instance'Access;
end Rx.Count;
