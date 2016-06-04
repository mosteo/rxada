with Rx.Subscriptions;
with Rx.Typed;

generic
   type T (<>) is private;
package Rx.Observable is

   package Typed is new Rx.Typed (T);

   subtype Observable is Typed.Producers.Observable'Class;
   subtype Observer   is Typed.Consumers.Observer'Class;

   function Just (V : T) return Observable'Class;

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Observer'Class;

   --  Last call, causes a subscription
   function "&" (L : Observable'Class;
                 R : Observer'Class)
                 return Subscriptions.Subscription;

   Chain : Subscriptions.Subscription;
   -- Never used, but there since something has to be done with the result of &

private

--     type Observable is new Typed.Producers.Observable with null record;
--
--     overriding
--     procedure Subscribe (Producer : in out Observable;
--                          Consumer : Typed.Consumers.Observer'Class) is null;

end Rx.Observable;
