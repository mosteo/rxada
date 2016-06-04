with Rx.Just;
with Rx.Subscribe;

package body Rx.Observable is

   package RxJust      is new Rx.Just (Typed);
   package RxSubscribe is new Rx.Subscribe (Typed);

   function Just (V : T) return Observable'Class is
   begin
      return RxJust.Create (V);
   end Just;

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Observer'Class is
   begin
      return RxSubscribe.As (On_Next);
   end Subscribe;

   --  Last call, causes a subscription
   function "&" (L : Typed.Producers.Observable'Class; R : Typed.Consumers.Observer'Class)
                 return Subscriptions.Subscription
   is
      Actual : Observable'Class := L;
   begin
      Actual.Subscribe (R);
      return Chain;
   end "&";

end Rx.Observable;
