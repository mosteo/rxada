with Rx.Tools.Holders;
with Rx.Impl.Transformers;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Impl.Preservers with Preelaborate is

   -- Specialized Transform, but with type preservation
   -- A separate package is convenient to allow independent package files for this kind of operators

   -- Shortcuts & bug workarounds
   subtype T          is Typed.Type_Traits.T;
   subtype Observable is Typed.Observable'Class;
   subtype Observer   is Typed.Observer'Class;

   package Transform is new Rx.Impl.Transformers (Typed, Typed);
   --  Specialization with type preservation here

   subtype Operator is Transform.Operator;
   --  Specialization of the Transformer type

   package From renames Transform.From;
   package Into renames Transform.Into;

   function Identity (V : From.T) return Into.T is (V);

   function Concatenate (Producer : Typed.Contracts.Observable'Class;
                          Consumer : Transform.Operator'Class)
                          return Typed.Observable renames Transform.Concatenate;
   -- Shortcut for simpler use elsewhere (particularly in Rx.Observables)

   package Linkers renames Transform.Linkers;
   --  Useable package

   package Holders is new Rx.Tools.Holders (Operator'Class, "operator'class");

end Rx.Impl.Preservers;
