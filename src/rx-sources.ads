with Rx.Subscriptions;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Sources is

   function Just (V : Typed.T) return Typed.Producers.Observable'Class;

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Typed.Consumers.Observer'Class;

   --  Last call, causes a subscription
   function "&" (L : Typed.Producers.Observable'Class;
                 R : Typed.Consumers.Observer'Class)
                 return Subscriptions.Subscription;

end Rx.Sources;
