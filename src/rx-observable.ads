with Rx.Subscriptions;
with Rx.Typed;

generic
   type T (<>) is private;
package Rx.Observable is

   package Typed is new Rx.Typed (T);

   subtype Observable is Typed.Producers.Observable'Class;
   subtype Observer   is Typed.Consumers.Observer'Class;

--     type Mutator is abstract new -- a Mutator changes the values but not their type
--       Typed.Producers.Subscriptor and
--       Typed.Producers.Observable
--     with null record; -- not sure i will be needing this

   function Just (V : T) return Observable'Class; -- what does using 'Class here? 'Class'Class?

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Observer'Class;

   --  Not last call sets R parent to L
--     function "&" (L : Observable'Class;
--                   R : Mutator'Class) return Observable'Class;

   --  Last call, causes a subscription
   function "&" (L : Observable'Class;
                 R : Observer'Class)
                 return Subscriptions.Subscription;

   Chain : Subscriptions.Subscription;
   -- Never used, but declared as convenience since something has to be done with the result of &

end Rx.Observable;
