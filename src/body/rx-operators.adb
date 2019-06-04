with Rx.Op.Length;
with Rx.Tools.Holders;

package body Rx.Operators is

   --------------
   -- Flat_Map --
   --------------

   package Definite_Operators is new Tools.Holders (Into.Operator);

   type Inflater is new Typed.Actions.TInflater1 with record
      Func     : Typed.Actions.Inflater1;
      Operator : Definite_Operators.Definite;
   end record;

   overriding function Evaluate (This : Inflater; V : From.T) return Into.Observable is
      use Into.Linkers;
   begin
      return
        This.Func (V)
        & This.Operator.Get;
   end Evaluate;

   function Flat_Map (Func     : Typed.Actions.Inflater1;
                      Pipeline : Into.Observable'Class)
                      return Typed.Operator'Class is
     (RxFlatMap.Create (Inflater'(Func     => Func,
                                  Operator => Definite_Operators.Hold
                                    (Into.Operator'Class (Pipeline)))));

   ------------
   -- Length --
   ------------

   function Length return Typed_Lists.Operator'Class is
      package RxLength is new Op.Length (Typed_Lists, Length);
   begin
      return RxLength.Create;
   end Length;

   ----------
   -- Size --
   ----------

   function Size return Operator'Class is
      package RxLength is new Op.Length (Typed, Size);
   begin
      return RxLength.Create;
   end Size;

end Rx.Operators;
