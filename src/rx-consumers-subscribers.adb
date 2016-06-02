package body Rx.Consumers.Subscribers is

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (This : in out Subscriber;
      Parent : Producers.Observable'Class)
   is
   begin
      This.Parent := Producers.To_Holder (Parent);
   end Set_Parent;

end Rx.Consumers.Subscribers;
