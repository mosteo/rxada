package body Rx.Base_Op is

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Operator;
      V : Typed.From.Type_Traits.T)
   is
   begin
      On_Next (V, This.Get_Child);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Typed.Operator'Class is
   begin
      return Operator'(Typed.Operator with null record);
   end Create;

end Rx.Base_Op;
