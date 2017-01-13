with Rx.Contracts;
with Rx.Tools.Holders;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Impl.Holders with Preelaborate is

   package Observables is new Rx.Tools.Holders (Contracts.Observable'Class);
   type Observable is new Observables.Definite with null record;

   package Observers is new Rx.Tools.Holders (Contracts.Observer'Class);
   type Observer is new Observers.Definite with null record;

   package Subscribers is new Rx.Tools.Holders (Contracts.Subscriber'Class);
   type Subscriber is new Subscribers.Definite with null record;

end Rx.Impl.Holders;
