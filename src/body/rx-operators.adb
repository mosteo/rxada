with Rx.Op.Length;

package body Rx.Operators is

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
