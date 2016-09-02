package body Rx.Map is

   type Op (F : Typed.Func1) is new Typed.Operator with null record;

   overriding
   procedure On_Next (This  : in out Op;
                      V     : Typed.From.Type_Traits.T) is
   begin
      This.Get_Child.On_Next (This.F (V));
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (F : Typed.Func1) return Typed.Operator'Class is
   begin
      return Op'(Typed.Operator with F => F);
   end Create;

end Rx.Map;
