package body Rx.Actions.Typed is

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (P : Proc1;
      V : Value'Class)
   is
   begin
      P.Raw (Values.Value'Class (V).Unwrap);
   end Call;

end Rx.Actions.Typed;
