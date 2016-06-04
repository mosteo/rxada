with Rx.Transform;
with Rx.Observable;

generic
   with package From is new Rx.Observable (<>); -- Naming chosen for same lenght
   with package Into is new Rx.Observable (<>);
package Rx.Operators is

   package Typed is new Transform (From.Typed, Into.Typed);

   function Map (F : Typed.Func1) return Typed.Operator'Class;

end Rx.Operators;
