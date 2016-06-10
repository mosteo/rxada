with Rx.Count;
with Rx.Map;
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

   subtype Operator Is Typed.Operator'Class;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Observable; R : Operator) return Into.Observable
     renames Typed."&";

   --------------
   -- Counters --
   --------------

   generic
      with function Succ (V : Into.T) return Into.T;
   package Counters is
      package Pkg_Count is new Rx.Count (Typed, Succ);
      function Count (First : Into.T) return Operator renames Pkg_Count.Count;
   end Counters;

   ---------
   -- Map --
   ---------

   function Map (F : Typed.Func1) return Operator;

private

   package RxMap is new Rx.Map (Typed);
   function Map (F : Typed.Func1) return Operator renames RxMap.Create;

end Rx.Operators;
