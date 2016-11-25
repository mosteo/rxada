with Rx.Holders;
with Rx.Transform;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Preserve with Preelaborate is

   -- Specialized Transform, but with type preservation
   -- A separate package is convenient to allow independent package files for this kind of operators

   -- Shortcuts
   subtype T          is Typed.Type_Traits.T;
   subtype Observable is Typed.Contracts.Observable'Class;
   subtype Observer   is Typed.Contracts.Observer'Class;
   subtype Subscriber is Typed.Contracts.Subscriber'Class;

   -- Scaffolding
   package Transform is new Rx.Transform (Typed, Typed);

   -- Not needed but works around some gnat bug on instantiations
   type Operator is abstract new Transform.Operator with null record;

   subtype Preserver is Operator'Class; -- An operator that does not changes the types involved

   package From renames Transform.From;
   package Into renames Transform.Into;

   package Holders is new Rx.Holders (Operator'Class, "operator");

end Rx.Preserve;
