with Rx.Holders;
with Rx.Transformers;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Preservers with Preelaborate is

   -- Specialized Transform, but with type preservation
   -- A separate package is convenient to allow independent package files for this kind of operators

   -- Shortcuts

   subtype T          is Typed.Type_Traits.T;
   subtype Observable is Typed.Observable;
   subtype Observer   is Typed.Observer;
   subtype Subscriber is Typed.Subscriber;

   package Transform is new Rx.Transformers (Typed, Typed);
   --  Specialization with type preservation here

   -- Not needed but works around some gnat bug on instantiations
   type Preserver is abstract new Transform.Transformer with null record;

   subtype Operator is Preserver'Class; -- An operator that does not changes the types involved

   package From renames Transform.From;
   package Into renames Transform.Into;

   package Holders is new Rx.Holders (Preserver'Class, "operator");

end Rx.Preservers;
