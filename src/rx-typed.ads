with Rx.Actions;
with Rx.Actions.Typed;
with Rx.Contracts;
with Rx.Defaults;
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

   package Defaults is new Rx.Defaults (Contracts);

   package Definite_Observables is new Impl.Definite_Observables (Contracts);

   package Conversions is

   --  This package is purely to work around gnat bugs on visibility and instantiation of tagged with untagged view

      function "=" (L, R : T) return Boolean renames Type_Traits."=";

      function "+" (V : T) return D renames Type_Traits.To_Definite;
      function "+" (V : D) return T renames Type_Traits.To_Indefinite;

      function Def (V : T) return D renames Type_Traits.To_Definite;
      function Ind (V : D) return T renames Type_Traits.To_Indefinite;

   end Conversions;

   package Holders is

      package Observers is new Rx.Holders (Observer'Class);
      type Observer is new Observers.Definite with null record;

      package Subscribers is new Rx.Holders (Subscriber'Class);
      type Subscriber is new Subscribers.Definite with null record;

   end Holders;

end Rx.Typed;
