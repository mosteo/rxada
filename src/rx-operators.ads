with Rx.Transform;
with Rx.Types;

generic
   with package From is new Rx.Types (<>); -- Naming chosen for same length
   with package Into is new Rx.Types (<>);
package Rx.Operators is

   package Typed is new Transform (From.Typed, Into.Typed);

   --  Types transformation magic happens here when chaining things
   function "&" (L : From.Typed.Producers.Observable'Class;
                 R : Typed.Operator'Class)
                 return Into.Typed.Producers.Observable'Class renames Typed."&";

   function Map (F : Typed.Func1) return Typed.Operator'Class;

end Rx.Operators;
