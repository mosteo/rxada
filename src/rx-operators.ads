with Rx.Observables;
with Rx.Transform;

generic
   --  These could well be trait packages, but using those the user only has to know about "observables" packages
   with package From is new Rx.Observables (<>); -- Naming chosen for same length
   with package Into is new Rx.Observables (<>);
package Rx.Operators is

-- This package seems unnecessary but by separating it from Transform we can too separate each operator
-- implementation classes in its own packages, just like with Typed/Observables hierarchy.

   package Typed is new Rx.Transform (From.Typed, Into.Typed);

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Observable; R : Typed.Operator'Class) return Into.Observable
     renames Typed."&";

   ---------
   -- Map --
   ---------

   function Map (F : Typed.Func1) return Typed.Operator'Class;

end Rx.Operators;
