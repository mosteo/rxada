with Rx.Observables;
with Rx.Transformers;

generic
   with package From is new Rx.Observables (<>);
   with package Into is new Rx.Observables (<>);
   with function To_Numeric (I : Long_Long_Integer) return Into.T;
   with function Succ (V : Into.T) return Into.T;
package Rx.Numeric_Operators is

   package Transformers        is new Rx.Transformers (From.Typed,       Into.Typed);
   package N_To_1_Transformers is new Rx.Transformers (From.Typed_Lists, Into.Typed);

   subtype Operator is Transformers.Operator;

   function Count (First : Into.T := To_Numeric (0)) return Operator;

   function Count (First : Into.T := To_Numeric (0)) return N_To_1_Transformers.Operator;

   function Length return N_To_1_Transformers.Operator;

end Rx.Numeric_Operators;
