with Rx.Transform;

generic
   with package Typed is new Rx.Transform (<>);
package Rx.Op.Map is

   function Create (F : Typed.Func1) return Typed.Operator'Class;

end Rx.Op.Map;
