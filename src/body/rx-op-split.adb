package body Rx.Op.Split is

   type Operator is new Transformer.Transformer with null record;

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Transformer.From.T;
                                 Child : in out Transformer.Observer)
   is
      pragma Unreferenced (This);
      procedure For_Each (V : Transformer.Into.T) is
      begin
         Child.On_Next (V);
      end For_Each;
   begin
      Iterate (V, For_Each'Access);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Transformer.Operator is
   begin
      return O : Operator;
   end Create;

end Rx.Op.Split;
