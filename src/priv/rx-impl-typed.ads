with Rx.Actions;
with Rx.Actions.Typed;
with Rx.Contracts;
with Rx.Conversions;
with Rx.Defaults;
with Rx.Factories;
with Rx.Impl.Definite_Observables;
with Rx.Impl.Definite_Observers;
with Rx.Impl.Holders;
with Rx.Traits.Types;

generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Impl.Typed with Preelaborate is

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
   package Definite_Observers   is new Impl.Definite_Observers (Contracts);
   package Conversions          is new Rx.Conversions (Type_Traits);
   package Factories            is new Rx.Factories (Contracts);
   package Holders              is new Impl.Holders (Contracts);

end Rx.Impl.Typed;
