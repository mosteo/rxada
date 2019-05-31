with Rx.Observables;
with Rx.Schedulers;

private with Rx.Op.Count;
private with Rx.Op.Length;
private with Rx.Src.Interval;
private with Rx.Src.Ranges;

generic
   with package Observables is new Rx.Observables (<>);
   with function To_Numeric (I : Rx_Integer) return Observables.T;
   with function Succ (V    : Observables.T) return Observables.T is <>;
   with function "<"  (L, R : Observables.T) return Boolean is <>;
package Rx.Numeric_Observables is

   --  Shortcuts

   subtype Observable is Observables.Observable'Class;
   subtype Operator   is Observables.Operator'Class;
   subtype T          is Observables.T;

   package From_List_Transformers renames Observables.From_List_Transformers;

   --  Operators

   -----------
   -- Count --
   -----------

   function Count (First : T := To_Numeric (0)) return Operator;

   function Count (First : T := To_Numeric (0))
                   return From_List_Transformers.Operator'Class;

   --------------
   -- Interval --
   --------------

   function Interval (First       : T;
                      Pause       : Duration             := 1.0;
                      First_Pause : Duration             := 0.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Observable;

   ------------
   -- Length --
   ------------

   function Length return From_List_Transformers.Operator'Class;

   ------------
   -- Ranges --
   ------------

   function Range_Count (First : T;
                         Count : Rx_Natural) return Observable;

   function Range_Slice (First : T;
                         Last  : T) return Observable;

private

   function List_Length (L : Observables.T_List) return T is
     (To_Numeric (Rx_Integer (L.Length)));

   package RxCount      is new Rx.Op.Count (Observables.Operate.Transform, Succ, To_Numeric (0));
   package RxCountLists is new Rx.Op.Count (From_List_Transformers, Succ, To_Numeric (0));
   package RxInterval   is new Rx.Src.Interval (Observables.Typed, Succ);
   package RxLength     is new Rx.Op.Length (From_List_Transformers, List_Length);
   package RxRange      is new Rx.Src.Ranges (Observables.Typed, Succ, "<");

   function Count (First : T := To_Numeric (0)) return Operator renames RxCount.Count;

   function Count (First : T := To_Numeric (0))
                   return From_List_Transformers.Operator'Class renames RxCountLists.Count;

   function Interval (First       : T;
                      Pause       : Duration := 1.0;
                      First_Pause : Duration := 0.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Observable renames RxInterval.Create;

   function Length return From_List_Transformers.Operator'Class renames RxLength.Create;

   function Range_Count (First : T;
                         Count : Rx_Natural) return Observable renames RxRange.From_Count;

   function Range_Slice (First : T;
                         Last  : T) return Observable renames RxRange.From_Slice;

end Rx.Numeric_Observables;
