with Rx.Operators;

private with Rx.Op.Count;
private with Rx.Op.Length;

generic
   with package Operators is new Rx.Operators (<>);
   with function To_Numeric (I : Rx_Integer) return Operators.Into.T;
   with function Succ (V : Operators.Into.T) return Operators.Into.T;
package Rx.Numeric_Operators is

   package From renames Operators.From;
   package Into renames Operators.Into;

   package Transformers           renames Operators.Typed;
   package From_List_Transformers renames Operators.Typed_Lists;

   subtype Operator is Transformers.Operator'Class;

   function Count (First : Into.T := To_Numeric (0)) return Operator;

   function Count (First : Into.T := To_Numeric (0)) return From_List_Transformers.Operator'Class;

   function Length return From_List_Transformers.Operator'Class;

private

   function List_Length (L : From.T_List) return Into.T is
     (To_Numeric (Rx_Integer (L.Length)));

   package RxCount      is new Rx.Op.Count (Transformers, Succ, To_Numeric (0));
   package RxCountLists is new Rx.Op.Count (From_List_Transformers, Succ, To_Numeric (0));
   package RxLength     is new Rx.Op.Length (From_List_Transformers, List_Length);

   function Count (First : Into.T := To_Numeric (0)) return Operator renames RxCount.Count;

   function Count (First : Into.T := To_Numeric (0))
                   return From_List_Transformers.Operator'Class renames RxCountLists.Count;

   function Length return From_List_Transformers.Operator'Class renames RxLength.Create;

end Rx.Numeric_Operators;
