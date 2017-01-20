with Rx.Tools.Holders;
with Rx.Transformers;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Preservers with Preelaborate is

   -- Specialized Transform, but with type preservation
   -- A separate package is convenient to allow independent package files for this kind of operators

   -- Shortcuts & bug workarounds
   subtype T          is Typed.Type_Traits.T;
   subtype Observable is Typed.Observable'Class;
   subtype Observer   is Typed.Observer'Class;

   package Transform is new Rx.Transformers (Typed, Typed);
   --  Specialization with type preservation here

   subtype Operator is Transform.Operator;
   --  Specialization of the Transformer type

   package From renames Transform.From;
   package Into renames Transform.Into;

   function Will_Observe (Producer : Typed.Contracts.Observable'Class;
                          Consumer : Transform.Operator'Class)
                          return Typed.Observable renames Transform.Will_Observe;
   -- Shortcut for simpler use elsewhere (particularly in Rx.Observables)

   package Holders is new Rx.Tools.Holders (Operator'Class, "operator");

end Rx.Preservers;
