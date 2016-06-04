with Rx.Actions;
with Rx.Producers;
with Rx.Subscriptions;

generic
   type T is private;
package Rx.Observable is

   package Actions   is new Rx.Actions (T);
   package Producers is new Rx.Producers (T);
   package Consumers renames Producers.Consumers;

   type Observable is new Producers.Observable with private;

   function Just (V : T) return Producers.Observable'Class;

--     procedure Subscribe (O        : Observable;
--                          On_Next  : Typed_Actions.Typed_Proc1 := null) is null;

   --  Last call, causes a subscription
--   function "&" (L : Base.Observable'Class; R : Consumer) return Subscriptions.Subscription;

   Chain : Subscriptions.Subscription;
   -- Never used, but there since something has to be done with the result of &

private

   type Observable is new Producers.Observable with null record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Consumers.Observer'Class) is null;

end Rx.Observable;
