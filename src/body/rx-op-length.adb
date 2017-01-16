package body Rx.Op.Length is

   type Transformer is new Transform.Operator with null record;

   overriding procedure On_Next (This  : in out Transformer;
                                 V     :        Transform.From.T)
   is
   begin
      This.Get_Observer.On_Next (Length (V));
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Transform.Operator'Class is
   begin
      return Transformer'(Transform.Operator with null record);
   end Create;

end Rx.Op.Length;
