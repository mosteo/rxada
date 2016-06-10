with Rx.Transform;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Operate is

   -- Operate is like Transform, but with type preservation
   -- A separate package is convenient to allow independent package files for this kind of operators

   -- Shortcuts
   subtype T          is Typed.Type_Traits.T;
   subtype Observable is Typed.Producers.Observable'Class;
   subtype Observer   is Typed.Consumers.Observer'Class;

   -- Scaffolding
   package Transform is new Rx.Transform (Typed, Typed);
   subtype Operator is Transform.Operator'Class;

end Rx.Operate;
