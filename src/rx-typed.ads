with Rx.Actions;
with Rx.Actions.Typed;
with Rx.Contracts;
with Rx.Conversions;
with Rx.Defaults;
with Rx.Factories;
with Rx.Holders;
with Rx.Impl.Definite_Observables;
with Rx.Traits.Types;

generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Typed is

   pragma Preelaborate;

   package Contracts is new Rx.Contracts (Type_Traits.T);
   --  The beginning of it all

   package Actions   is new Rx.Actions.Typed (Type_Traits.T);

   -- Shortcuts
   subtype T is Type_Traits.T;
   subtype D is Type_Traits.D;
   subtype Observable is Contracts.Observable'Class;
   subtype Observer   is Contracts.Observer'Class;
   subtype Sink       is Contracts.Sink'Class;
   subtype Subscriber is Contracts.Subscriber'Class;

   -- Typed packages for use with an Rx type

   package Defaults             is new Rx.Defaults (Contracts);
   package Definite_Observables is new Impl.Definite_Observables (Contracts);
   package Conversions          is new Rx.Conversions (Type_Traits);
   package Factories            is new Rx.Factories (Contracts);

   package Holders is

      package Observables is new Rx.Holders (Observable'Class);
      type Observable is new Observables.Definite with null record;

      package Observers is new Rx.Holders (Observer'Class);
      type Observer is new Observers.Definite with null record;

      package Subscribers is new Rx.Holders (Subscriber'Class);
      type Subscriber is new Subscribers.Definite with null record;

   end Holders;

end Rx.Typed;
