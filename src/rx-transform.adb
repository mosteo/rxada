with Rx.Values;

package body Rx.Transform is

   type Func1_Wrapper (Cooked : Func1) is new Actions.Func1 with null record;

   overriding
   function Call (F : Func1_Wrapper; V : Rx.Values.Value'Class) return Rx.Values.Value'Class is
   begin
      return Consumer.Values.Wrap (F.Cooked (Producer.Values.Value'Class (V).Unwrap));
   end Call;

   ----------
   -- Func --
   ----------

   function Func (F : Func1) return Rx.Actions.Func1'Class is
   begin
      return Func1_Wrapper'(Cooked => F);
   end Func;

end Rx.Transform;
