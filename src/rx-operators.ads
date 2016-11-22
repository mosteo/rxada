with Rx.Observables;
with Rx.Op.Count;
with Rx.Transform;

private with Rx.Op.Map;
private with Rx.Op.Scan;

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
   renames Typed.Will_Observe;

   --  From here on, instances of operators that transform between two types

   --------------
   -- Counters --
   --------------

   generic
      with function Succ (V : Into.T) return Into.T;
      Default_Initial_Count : Into.T;
   package Counters is
      package Pkg_Count is new Rx.Op.Count (Typed, Succ, Default_Initial_Count);
      function Count (First : Into.T) return Operator renames Pkg_Count.Count;
   end Counters;

   ---------
   -- Map --
   ---------

   function Map (F : Typed.Actions.Func1) return Operator;

   ----------
   -- Scan --
   ----------

   function Scan (F         : Typed.Actions.Func2;
                  Seed      : Into.T;
                  Emit_Seed : Boolean) return Operator;

private

   package RxMap is new Rx.Op.Map (Typed);
   function Map (F : Typed.Actions.Func1) return Operator renames RxMap.Create;

   package RxScan is new Rx.Op.Scan (Typed);
   function Scan (F         : Typed.Actions.Func2;
                  Seed      : Into.T;
                  Emit_Seed : Boolean) return Operator renames RxScan.Create;

end Rx.Operators;
