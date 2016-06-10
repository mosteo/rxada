with Rx.Observables;
with Rx.Schedulers;
with Rx.Transform;

generic
   --  These could well be trait packages, but using those the user only has to know about "observables" packages
   with package From is new Rx.Observables (<>); -- Naming chosen for same length
   with package Into is new Rx.Observables (<>);
package Rx.Operators is

   package Typed is new Transform (From.Typed, Into.Typed);

   ---------
   -- "&" --
   ---------
   --  Types transformation magic happens here when chaining things
   function "&" (L : From.Typed.Producers.Observable'Class;
                 R : Typed.Operator'Class)
                 return Into.Typed.Producers.Observable'Class renames Typed."&";

   ---------
   -- Map --
   ---------

   function Map (F : Typed.Func1) return Typed.Operator'Class;

   ----------------
   -- Observe_On --
   ----------------

   function Observe_On (Scheduler : Schedulers.Scheduler) return Typed.Operator'Class;

end Rx.Operators;
