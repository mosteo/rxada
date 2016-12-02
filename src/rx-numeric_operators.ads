with Rx.Observables;
with Rx.Transformers;

private with Rx.Op.Count;
private with Rx.Op.Length;

generic
   with package From is new Rx.Observables (<>);
   with package Into is new Rx.Observables (<>);
   with function To_Numeric (I : Rx_Integer) return Into.T;
   with function Succ (V : Into.T) return Into.T;
package Rx.Numeric_Operators is

   package Transformers           is new Rx.Transformers (From.Typed,       Into.Typed);
   package From_List_Transformers is new Rx.Transformers (From.Typed_Lists, Into.Typed);

   subtype Operator is Transformers.Operator;

   function Count (First : Into.T := To_Numeric (0)) return Operator;

   function Count (First : Into.T := To_Numeric (0)) return From_List_Transformers.Operator;

   function Length return From_List_Transformers.Operator;

private

   function List_Length (L : From.T_List) return Into.T is (To_Numeric (Rx_Integer (L.Length))) with Inline;

   package RxCount      is new Rx.Op.Count (Transformers, Succ, To_Numeric (0));
   package RxCountLists is new Rx.Op.Count (From_List_Transformers, Succ, To_Numeric (0));
   package RxLength     is new Rx.Op.Length (From_List_Transformers, List_Length);

   function Count (First : Into.T := To_Numeric (0)) return Operator renames RxCount.Count;

   function Count (First : Into.T := To_Numeric (0))
                   return From_List_Transformers.Operator renames RxCountLists.Count;

   function Length return From_List_Transformers.Operator renames RxLength.Create;

end Rx.Numeric_Operators;
