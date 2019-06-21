with Rx.Observables;
with Rx.Op.Count;
with Rx.Impl.Transformers;

private with Rx.Op.Flatmap;
private with Rx.Op.Map;
private with Rx.Op.Scan;

generic
   --  These could well be trait packages, but using those the user only has to know about "observables" packages
   with package From is new Rx.Observables (<>); -- Naming chosen for same length
   with package Into is new Rx.Observables (<>);
package Rx.Operators is

-- This package seems unnecessary but by separating it from Transform we can too separate each operator
-- implementation classes in its own packages, just like with Typed/Observables hierarchy.

   package Typed is new Rx.Impl.Transformers (From.Typed, Into.Typed);
   package Typed_Lists is new Rx.Impl.Transformers (From.Typed_Lists, Into.Typed);

   subtype Operator Is Typed.Operator'Class;

   procedure Diagnose (This : Into.Observable'Class);
   --  Experimental dump of This chain
   --  The chain is expected to be formed by
   --    From.Operator'Class, (any amount)
   --    Typed.Operator'Class,(one)
   --    Into.Operator'Class, (any amount)

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

   --------------
   -- Flat_Map --
   --------------

   function Flat_Map (Func : Typed.Actions.Inflater1)
                      return Typed.Operator'Class;

   function Flat_Map (Func : Typed.Actions.TInflater1'Class)
                      return Typed.Operator'Class;

   function Flat_Map (Pipeline : Into.Observable'Class)
                      return Typed.Operator'Class;
   --  Subscribes to Just (V) & Pipeline
   --  Pipeline must be of ...-AA-AA-AB-BB-BB-... for types to match at runtime
   --  Pipeline must be made of all Operator'Class
   --  See notes in Rx.Op.Flat_Map for more detail

   ------------
   -- Length --
   ------------

   generic
      with function Length (V : From.Typed_Lists.T) return Into.T;
   function Length return Typed_Lists.Operator'Class;

   ---------
   -- Map --
   ---------

   function Map (F : Typed.Actions.Func1) return Operator'Class;

   ----------
   -- Scan --
   ----------

   function Scan (F         : Typed.Actions.Func2;
                  Seed      : Into.T;
                  Emit_Seed : Boolean) return Operator'Class;

   ----------
   -- Size --
   ----------

   generic
      with function Size (V : From.T) return Into.T;
   function Size return Operator'Class;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Observable; R : Operator) return Into.Observable
                 renames Typed.Concatenate;

   function "&" (L : From.Observable; R : Typed.Actions.Func1) return Into.Observable;
   --  Implicit Map

   function "&" (Producer : From.Observable'Class;
                 Consumer : Typed.Actions.Inflater1)
                 return Into.Observable'Class;
   --  Implicit Flat_Map

   function "&" (Producer : From.Observable'Class;
                 Consumer : Typed.Actions.TInflater1'Class)
                 return Into.Observable'Class;
   --  Implicit Flat_Map

   package Linkers is

      --  Analog to the one in Observables

      function "&" (L : From.Observable; R : Operator) return Into.Observable
                    renames Typed.Concatenate;

      function "&" (L : From.Observable; R : Typed.Actions.Func1) return Into.Observable
                    renames Operators."&";

      function "&" (Producer : From.Observable'Class;
                    Consumer : Typed.Actions.Inflater1)
                    return Into.Observable'Class renames Operators."&";

      function "&" (Producer : From.Observable'Class;
                    Consumer : Typed.Actions.TInflater1'Class)
                    return Into.Observable'Class renames Operators."&";

   end Linkers;

private

   function Identity (Unused : Typed.From.Observer'Class) return Typed.Into.Observer'Class is
     (raise Program_Error with "identity unavailable in Transformer context");

   procedure Set_Parent (This   : in out Into.Observable'Class;
                         Parent :        From.Observable'Class);
   --  Cross-type Concatenate: used to be able to supply AA-AA-AB-BB-BB chains
   --  to Flat_Map. Thus, at some point in the upstream of This, an AB operator
   --  must exist, with optional AA before it, that can observe givent Parent.
   --  Here, we navigate upstream from This (all must be Operator'Class), and
   --  finally set Parent as parent of the first upstream.
   --  See also Diagnose, which does something similar (simpler) for diagnostics.

   ---------------
   -- RxFlatMap --
   ---------------

   package RxFlatMap is new Rx.Op.Flatmap (Typed,
                                           Identity, Typed.Broken_Identity,
                                           Set_Parent);
   function Flat_Map (Func : Typed.Actions.Inflater1)
                      return Typed.Operator'Class is
      (RxFlatMap.Create (Func, Recursive => False));
   function Flat_Map (Func : Typed.Actions.TInflater1'Class)
                      return Typed.Operator'Class is
      (RxFlatMap.Create (Func, Recursive => False));
   function Flat_Map (Pipeline : Into.Observable'Class)
                      return Typed.Operator'Class is
     (RxFlatMap.Create (Pipeline, Recursive => False));
   function "&" (Producer : From.Observable'Class;
                 Consumer : Typed.Actions.Inflater1)
                 return Into.Observable'Class renames RxFlatMap."&";
   function "&" (Producer : From.Observable'Class;
                 Consumer : Typed.Actions.TInflater1'Class)
                 return Into.Observable'Class renames RxFlatMap."&";

   -----------
   -- RxMap --
   -----------

   package RxMap is new Rx.Op.Map (Typed);
   function Map (F : Typed.Actions.Func1) return Operator renames RxMap.Create;
   function "&" (L : From.Observable; R : Typed.Actions.Func1) return Into.Observable renames RxMap."&";

   ------------
   -- RxScan --
   ------------

   package RxScan is new Rx.Op.Scan (Typed);
   function Scan (F         : Typed.Actions.Func2;
                  Seed      : Into.T;
                  Emit_Seed : Boolean) return Operator renames RxScan.Create;

end Rx.Operators;
