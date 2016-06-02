package body Rx.Actions.Typed is

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (P : Proc1;
      V : Value'Class)
   is
   begin
      if P.Raw /= null then
         P.Raw (Values.Value'Class (V).Unwrap);
      end if;
   end Call;

end Rx.Actions.Typed;
