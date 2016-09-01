with Rx.Transform;

generic
   with package Typed is new Rx.Transform (<>);
   with procedure On_Next (V : Typed.From.Type_Traits.T; Child : in out Typed.Into.Observer'Class);
package Rx.Base_Op is

   type Operator is new Typed.Operator with null record;

   overriding procedure On_Next (This : in out Operator; V : Typed.From.Type_Traits.T);

   function Create return Typed.Operator'Class;

end Rx.Base_Op;
