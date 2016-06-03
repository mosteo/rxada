with Rx.Actions;
with Rx.Actions.Typed;
with Rx.Base;
with Rx.Consumers;
with Rx.Producers;
with Rx.Subscriptions;
private with Rx.Values;
with Rx.Values.Typed;

generic
   type T (<>) is private;
package Rx.Observable is

   package Values   is new Rx.Values.Typed (T);
   package Typed_Actions is new Rx.Actions.Typed (Values);

   type Observable is new Base.Observable with private;

   function Just (V : T) return Observable'Class;

   procedure Subscribe (O        : Observable;
                        On_Next  : Typed_Actions.Typed_Proc1 := null);

   function Proc (P : Typed_Actions.Typed_Proc1) return Rx.Actions.Proc1'Class is
     (Typed_Actions.Proc1'(Raw => P));

   --  END OF DOT NOTATION USAGE AND START OF & USAGE  --

   type Consumer is new Consumers.Observer with private;

   function Subscribe (On_Next  : Typed_Actions.Typed_Proc1 := null) return Consumer;

   --  Last call, causes a subscription
   function "&" (L : Base.Observable'Class; R : Consumer) return Subscriptions.Subscription;

   Chain : Subscriptions.Subscription;
   -- Never used, but there since something has to be done with the result of &

private

   type Observable is new Base.Observable with record
      Untyped : Producers.Holder;
      -- This is used when this observable wraps a typed source that has no knowledge of its type (like just).
      -- This need is a side effect of having Just implementation in its own file... if it where here this wouldnt' be needed.
      -- This for now is a price I pay gladly to keep this file smallish, although this forces some duplication in Base too.
   end record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Consumers.Observer'Class);

   type Consumer is new Consumers.Observer with record
      Untyped : Consumers.Holder;
      -- Likewise, we use this type to wrap an untyped observer that we however know is of our type
   end record;

   overriding
   procedure OnNext (This : Consumer; V : Rx.Values.Value'Class);

end Rx.Observable;
